# Extreme Performance Optimization for 100K+ Records

## 🎯 TARGET: Sub-100ms load time for ALL tabs with 100K+ records

## 🔍 Current Bottlenecks

### Problem 1: Loading ALL Data at Once
- **Current**: Loading 100 cards per request
- **Issue**: With 100K cards, even 100 records is too slow
- **Solution**: Implement **cursor-based pagination** - load only 20-30 items initially

### Problem 2: No Data Caching
- **Current**: Every query hits MongoDB
- **Issue**: Even optimized queries take 20-50ms
- **Solution**: Add **Redis caching** for sub-10ms responses

### Problem 3: Heavy Rendering
- **Current**: Rendering all loaded data immediately
- **Issue**: React Native struggles with 100+ items
- **Solution**: Virtual scrolling with **aggressive windowing**

### Problem 4: Multiple Sequential Queries
- **Current**: Fetching photos/profiles separately
- **Issue**: Multiple round trips to database
- **Solution**: Store photo URLs in shared card document (denormalize more)

## 🚀 OPTIMIZATION STRATEGY

### Phase 1: Backend - Cursor-Based Pagination

#### Why Cursor Pagination?
- **Offset-based** (`skip(100)`): Gets SLOWER as offset increases
  - skip(0): 10ms
  - skip(1000): 50ms  
  - skip(10000): 500ms ❌
  
- **Cursor-based**: CONSTANT speed regardless of position
  - First page: 10ms
  - Page 1000: 10ms ✅

#### Implementation

```typescript
// Instead of: limit(100)
// Use: Cursor-based with _id

r.get("/sent", async (req: AuthReq, res) => {
  const senderId = req.userId!;
  const cursor = req.query.cursor as string; // Last _id from previous page
  const limit = parseInt(req.query.limit as string) || 20; // Default 20 items
  
  const query: any = { senderId };
  
  // If cursor provided, get items AFTER this cursor
  if (cursor) {
    query._id = { $lt: cursor }; // MongoDB can use index on _id
  }
  
  const sentCards = await SharedCard.find(query)
    .select('_id cardId recipientId recipientName cardTitle sentAt status message')
    .sort({ _id: -1 }) // Sort by _id (same as creation time)
    .limit(limit + 1) // Fetch one extra to check if there's more
    .lean();
  
  const hasMore = sentCards.length > limit;
  const items = sentCards.slice(0, limit);
  const nextCursor = hasMore ? items[items.length - 1]._id : null;
  
  return res.json({
    success: true,
    data: items,
    pagination: {
      nextCursor,
      hasMore,
      count: items.length
    }
  });
});
```

**Performance**: 10-15ms per page, ALWAYS (even at page 1000)

### Phase 2: Redis Caching

#### Cache Strategy
```
Key: "sent:userId:cursor:limit"
TTL: 30 seconds
Value: JSON stringified response
```

#### Implementation
```typescript
import Redis from 'ioredis';
const redis = new Redis(process.env.REDIS_URL);

// Before query:
const cacheKey = `sent:${senderId}:${cursor || 'first'}:${limit}`;
const cached = await redis.get(cacheKey);

if (cached) {
  return res.json(JSON.parse(cached)); // 3-5ms response!
}

// After query:
await redis.setex(cacheKey, 30, JSON.stringify(response));
```

**Performance**: 3-10ms (cached) vs 20-50ms (uncached)

### Phase 3: Frontend - Infinite Scroll

#### React Query Infinite Queries
```typescript
const sentCardsQuery = useInfiniteQuery({
  queryKey: ["sent-cards"],
  queryFn: async ({ pageParam = null }) => {
    const url = pageParam 
      ? `/cards/sent?cursor=${pageParam}&limit=20`
      : `/cards/sent?limit=20`;
    
    const response = await api.get(url);
    return response;
  },
  getNextPageParam: (lastPage) => {
    return lastPage.pagination?.nextCursor || undefined;
  },
  staleTime: 30 * 1000,
  gcTime: 5 * 60 * 1000,
});

// Access data:
const allCards = sentCardsQuery.data?.pages.flatMap(p => p.data) || [];

// Load more on scroll:
const loadMore = () => {
  if (sentCardsQuery.hasNextPage && !sentCardsQuery.isFetchingNextPage) {
    sentCardsQuery.fetchNextPage();
  }
};
```

#### FlatList with Infinite Scroll
```typescript
<FlatList
  data={allCards}
  onEndReached={loadMore}
  onEndReachedThreshold={0.5} // Load when 50% from bottom
  initialNumToRender={10} // Render only 10 initially
  maxToRenderPerBatch={10}
  windowSize={5} // Keep only 5 screens worth of items
  removeClippedSubviews={true}
  getItemLayout={getItemLayout} // Pre-calculate heights
/>
```

**Performance**: Renders instantly, loads more as needed

### Phase 4: Ultra Denormalization

#### Current Problem
```typescript
// 3 database queries for each page:
1. SharedCard.find() - 10ms
2. Card.find() for photos - 5ms  
3. User.find() for profiles - 5ms
Total: 20ms
```

#### Solution: Store EVERYTHING in SharedCard
```typescript
// When creating shared card:
const sharedCard = await SharedCard.create({
  cardId,
  senderId,
  recipientId,
  cardTitle: card.companyName,
  senderName: sender.name,
  recipientName: recipient.name,
  cardPhoto: card.companyPhoto, // ✅ Store photo URL
  senderProfilePicture: sender.profilePicture, // ✅ Store profile pic
  recipientProfilePicture: recipient.profilePicture, // ✅ Store profile pic
  // ... other fields
});
```

**Now only 1 query needed**:
```typescript
const sentCards = await SharedCard.find({ senderId })
  .select('_id cardTitle recipientName cardPhoto recipientProfilePicture sentAt status')
  .sort({ _id: -1 })
  .limit(20)
  .lean();
  
// No additional queries! Everything is in one document
```

**Performance**: 10ms (one query) vs 20ms (three queries) = **50% faster**

### Phase 5: Database Indexes

#### Critical Compound Indexes
```javascript
// For cursor-based pagination:
{ senderId: 1, _id: -1 }         // Sent cards with cursor
{ recipientId: 1, _id: -1 }      // Received cards with cursor
{ groupId: 1, _id: -1 }          // Group cards with cursor

// For status filtering:
{ recipientId: 1, status: 1, _id: -1 } // Unviewed cards
```

### Phase 6: Response Compression

```typescript
import compression from 'compression';

app.use(compression({
  level: 6, // Balanced compression
  threshold: 1024, // Only compress > 1KB
  filter: (req, res) => {
    if (req.headers['x-no-compression']) {
      return false;
    }
    return compression.filter(req, res);
  }
}));
```

**Performance**: 60% smaller payloads = faster network transfer

## 📊 Expected Performance (100K Records)

| Operation | Before | After | Improvement |
|-----------|---------|-------|-------------|
| **First Page Load** | 92ms | **10-15ms** | 84% faster |
| **Scroll to Page 10** | 200ms | **10-15ms** | 93% faster |
| **Scroll to Page 100** | 2000ms | **10-15ms** | 99% faster! |
| **With Redis Cache** | 92ms | **3-5ms** | 95% faster |
| **Network Transfer** | 50KB | **20KB** | 60% smaller |
| **Initial Render** | 500ms | **50ms** | 90% faster |

## 🎯 Implementation Priority

### High Priority (Immediate Impact)
1. ✅ **Cursor-based pagination** (Backend)
2. ✅ **Infinite scroll** (Frontend)
3. ✅ **Ultra denormalization** (Store photos in SharedCard)
4. ✅ **Compound indexes** (Database)

### Medium Priority (2x improvement)
5. **Redis caching** (Requires Redis setup)
6. **Response compression** (Quick win)

### Low Priority (Nice to have)
7. **Prefetching** (Load next page before scroll)
8. **Service workers** (Offline support)

## 🔧 Implementation Steps

### Step 1: Update SharedCard Schema
Add photo fields to model:
```typescript
cardPhoto: String,
senderProfilePicture: String,
recipientProfilePicture: String,
```

### Step 2: Update Card Sharing Logic
Store photos when creating shared card

### Step 3: Add Cursor Pagination
Update `/sent`, `/received`, `/groups` endpoints

### Step 4: Frontend Infinite Scroll
Convert useQuery to useInfiniteQuery

### Step 5: Database Indexes
Run index creation script

### Step 6: Add Redis (Optional)
Setup Redis caching layer

## ✅ Success Metrics

After implementation:
- ✅ First page loads in **< 20ms**
- ✅ Scrolling to page 100 takes **< 20ms**
- ✅ UI renders in **< 100ms**
- ✅ Works smoothly with **100K+ records**
- ✅ With Redis: **< 10ms** response times
- ✅ Network payload **< 30KB** per page

## 🚀 Result

The app will feel **INSTANT** even with 100K+ cards because:
1. Only 20 items load at a time
2. Cursor pagination is equally fast at any position
3. One database query per request
4. Redis caching gives sub-10ms responses
5. Virtual scrolling renders only visible items
6. Compressed responses transfer faster

**Users won't even notice if there are 100 or 100,000 cards!** 🎉
