# Send/Receive Tab Performance Issues - DIAGNOSIS & FIXES

## 🔍 Issue Identified

The send/receive tab is loading slowly because of multiple performance bottlenecks:

### 1. **Missing Database Indexes** ✅ FIXED
- **Problem**: The indexes exist in the schema but weren't properly created in production MongoDB
- **Impact**: Queries were doing full collection scans instead of using indexes
- **Solution**: Ran `fix-sent-received-indexes.js` to recreate all indexes
- **Status**: ✅ Indexes now created and optimized

### 2. **Multiple Populate Operations**
- **Problem**: Each query does 2 populate operations (cardId + recipientId/senderId)
- **Impact**: ~50-80% overhead from populate operations (79ms overhead on 92ms query)
- **Current Performance**: 
  - Sent cards: ~92ms for 3 cards (with populate)
  - Received cards: ~56ms for 1 card (with populate)
- **Optimization Needed**: Use lean() and cached denormalized data instead

### 3. **No Frontend Caching**
- **Problem**: React Query refetches on every mount/focus
- **Impact**: Unnecessary API calls even when data hasn't changed
- **Solution**: Add staleTime and cacheTime to React Query

### 4. **Backend Not Using Cached Data**
- **Problem**: Schema has cached fields (senderName, recipientName, cardTitle) but still doing populate
- **Impact**: Wasted denormalized data, slower queries
- **Solution**: Remove populate and use cached fields directly

## 🛠️ FIXES TO IMPLEMENT

### Fix 1: Optimize Backend Queries (Remove Unnecessary Populates)

**Current Code** (Slow):
```typescript
const sentCards = await SharedCard.find({ senderId })
  .populate('cardId', 'companyName name companyPhoto')
  .populate('recipientId', 'name profilePicture')
  .sort({ sentAt: -1 })
  .limit(100)
  .lean()
  .exec();
```

**Optimized Code** (Fast):
```typescript
// Option A: Use cached data (FASTEST)
const sentCards = await SharedCard.find({ senderId })
  .select('_id cardId recipientId recipientName cardTitle sentAt status message viewedAt')
  .sort({ sentAt: -1 })
  .limit(100)
  .lean()
  .exec();

// Then populate only cardPhoto separately if needed
const cardIds = [...new Set(sentCards.map(s => s.cardId))];
const cards = await Card.find({ _id: { $in: cardIds } })
  .select('_id companyPhoto')
  .lean();
const cardPhotoMap = Object.fromEntries(cards.map(c => [c._id.toString(), c.companyPhoto]));

// Then populate only profilePicture separately if needed
const recipientIds = [...new Set(sentCards.map(s => s.recipientId))];
const users = await User.find({ _id: { $in: recipientIds } })
  .select('_id profilePicture')
  .lean();
const profilePicMap = Object.fromEntries(users.map(u => [u._id.toString(), u.profilePicture]));

// Format response with cached + fetched data
const formattedCards = sentCards.map(share => ({
  _id: share._id,
  cardId: share.cardId,
  recipientId: share.recipientId,
  recipientName: share.recipientName, // From cached field
  recipientProfilePicture: profilePicMap[share.recipientId] || null,
  cardTitle: share.cardTitle, // From cached field
  cardPhoto: cardPhotoMap[share.cardId.toString()] || null,
  sentAt: share.sentAt,
  status: share.status,
  message: share.message,
  viewedAt: share.viewedAt
}));
```

**Performance Improvement**: ~60-80% faster (from 92ms to ~20-30ms)

### Fix 2: Add React Query Caching

**File**: `InstantllyCards/app/(tabs)/chats.tsx`

**Current Code**:
```typescript
const sentCardsQuery = useQuery({
  queryKey: ["sent-cards"],
  queryFn: async () => {
    // ...
  },
});
```

**Optimized Code**:
```typescript
const sentCardsQuery = useQuery({
  queryKey: ["sent-cards"],
  queryFn: async () => {
    // ...
  },
  staleTime: 30 * 1000, // Consider data fresh for 30 seconds
  cacheTime: 5 * 60 * 1000, // Keep in cache for 5 minutes
  refetchOnMount: false, // Don't refetch if data is fresh
  refetchOnWindowFocus: false, // Don't refetch on window focus
});
```

**Performance Improvement**: Eliminates unnecessary API calls

### Fix 3: Use Pagination/Lazy Loading

**Current**: Loading all 100 cards at once
**Optimized**: Load 20 cards initially, then lazy load more on scroll

```typescript
const sentCardsQuery = useInfiniteQuery({
  queryKey: ["sent-cards"],
  queryFn: async ({ pageParam = 0 }) => {
    const response = await api.get(`/cards/sent?limit=20&skip=${pageParam}`);
    return response?.data || [];
  },
  getNextPageParam: (lastPage, allPages) => {
    if (lastPage.length < 20) return undefined;
    return allPages.length * 20;
  },
  staleTime: 30 * 1000,
  cacheTime: 5 * 60 * 1000,
});
```

### Fix 4: Add Response Compression

**File**: `Instantlly-Cards-Backend/src/index.ts`

Add compression middleware:
```typescript
import compression from 'compression';

app.use(compression()); // Add before routes
```

**Performance Improvement**: ~40-60% smaller response size for large datasets

### Fix 5: Use Database Connection Pooling

Check MongoDB connection settings:
```typescript
mongoose.connect(process.env.MONGODB_URI, {
  maxPoolSize: 10, // Maximum number of connections
  minPoolSize: 2,  // Minimum number of connections
  socketTimeoutMS: 45000,
  serverSelectionTimeoutMS: 10000,
});
```

## 📊 Expected Performance After Fixes

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Sent cards query | 92ms | ~20-30ms | 70% faster |
| Received cards query | 56ms | ~15-25ms | 65% faster |
| Network payload | ~50KB | ~20KB | 60% smaller |
| Frontend refetches | Every mount | Once per 30s | 95% reduction |
| Total load time | 2-3s | ~0.5s | 75% faster |

## 🚀 Implementation Priority

1. ✅ **DONE**: Fix database indexes (completed)
2. **HIGH**: Optimize backend queries (remove unnecessary populates)
3. **HIGH**: Add React Query caching
4. **MEDIUM**: Add response compression
5. **LOW**: Implement pagination (for future scalability)

## 🧪 Testing After Fixes

Run these commands to verify performance:
```bash
# 1. Test backend query performance
node test-sent-received-performance.js

# 2. Test API endpoint directly
curl -X GET "https://instantlly-cards-backend-6ki0.onrender.com/api/cards/sent" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -w "\nTime: %{time_total}s\n"

# 3. Monitor backend logs for query times
node check-backend-logs.js
```

## 📌 Additional Optimizations (Future)

1. **Redis Caching**: Cache frequently accessed sent/received cards in Redis
2. **CDN for Images**: Serve card photos from CDN instead of MongoDB
3. **GraphQL**: Use GraphQL to fetch only needed fields
4. **Service Workers**: Cache API responses in browser
5. **WebSocket**: Real-time updates instead of polling

## ⚠️ Important Notes

- The `senderId` and `recipientId` fields are String type (not ObjectId) to support temporary userIds
- This means indexes work but mongoose.populate() is less efficient
- Using cached denormalized fields is MORE efficient than populate for String references
- Always use `.lean()` for read-only queries to skip Mongoose document overhead
