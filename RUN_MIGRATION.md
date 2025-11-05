# 🚀 GridFS Migration - Step-by-Step Instructions

## ✅ Prerequisites

- [x] Code deployed to Render (auto-deploy complete)
- [x] Backend is running and healthy
- [x] You have MongoDB connection string

---

## 📋 Migration Steps

### Step 1: Wait for Render Deploy (2-3 minutes)

Check deployment status at: https://dashboard.render.com

**Wait for:**
- ✅ Build complete
- ✅ Deploy successful
- ✅ Service running

**Verify backend is up:**
```bash
curl https://instantlly-cards-backend-6ki0.onrender.com/api/health
```

Should return: `{"ok":true,"database":"mongodb","dbStatus":"connected"...}`

---

### Step 2: Run Migration Locally (Recommended)

The migration script connects directly to your production MongoDB and migrates ads.

#### Option A: Run Locally (Fastest & Safest)

```bash
cd "/Users/muskaan7862407/Desktop/Instantlly app/Instantlly-Cards-Backend"

# Make sure you have the MongoDB URI in .env
# MONGODB_URI=mongodb+srv://...

# Run migration
npm run migrate:gridfs
```

**Expected Output:**
```
🚀 Starting migration: Base64 → GridFS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ Connected to MongoDB
✅ GridFS initialized

📊 Found 45 ads to migrate

[1/45] Migrating: Murtaza Cards
  ID: 672a0a85c1fd5e61889e5f2e
  📤 Uploading bottom image (1.19MB)...
  ✅ Bottom image uploaded: 673abc123...
  📤 Uploading fullscreen image (2.84MB)...
  ✅ Fullscreen image uploaded: 673def456...
  ✅ Ad updated with GridFS references

[2/45] Migrating: Next Ad...
...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Migration Complete!
  ✅ Success: 45 ads
  ❌ Failed: 0 ads

⚠️  IMPORTANT NEXT STEPS:
  1. Test the new /api/ads/active endpoint
  2. Verify mobile app can load images
  3. Run cleanup script to remove base64 data:
     npm run migrate:gridfs:cleanup
```

#### Option B: Run on Render Shell (Alternative)

If you need to run directly on Render:

1. Go to Render Dashboard → Your Service
2. Click "Shell" tab
3. Run:
   ```bash
   npm run migrate:gridfs
   ```

---

### Step 3: Test New API Endpoints

#### Test Metadata Endpoint (Should be FAST now)

```bash
curl -s -w "\n\n📊 Status: %{http_code}\n⏱️  Time: %{time_total}s\n📦 Size: %{size_download} bytes\n" \
  https://instantlly-cards-backend-6ki0.onrender.com/api/ads/active | head -50
```

**Expected Result:**
- Status: 200
- Time: <0.5s (was 19s!)
- Size: <100KB (was 66MB!)
- JSON response with image URLs

**Sample Response:**
```json
{
  "success": true,
  "data": [
    {
      "_id": "672a0a85c1fd5e61889e5f2e",
      "title": "Murtaza Cards",
      "phoneNumber": "+91 98674 77227",
      "bottomImageUrl": "/api/ads/image/672a0a85c1fd5e61889e5f2e/bottom",
      "fullscreenImageUrl": "/api/ads/image/672a0a85c1fd5e61889e5f2e/fullscreen",
      "hasBottomImage": true,
      "hasFullscreenImage": true
    }
  ],
  "count": 45,
  "imageBaseUrl": "https://instantlly-cards-backend-6ki0.onrender.com"
}
```

#### Test Image Streaming Endpoint

```bash
# Test bottom image (should stream JPEG)
curl -s -w "\n\n📊 Status: %{http_code}\n⏱️  Time: %{time_total}s\n📦 Size: %{size_download} bytes\n" \
  https://instantlly-cards-backend-6ki0.onrender.com/api/ads/image/672a0a85c1fd5e61889e5f2e/bottom \
  -o /tmp/test_ad_image.jpg

# Check if image downloaded
file /tmp/test_ad_image.jpg
# Should show: JPEG image data
```

**Expected Result:**
- Status: 200
- Time: <1s
- Size: 1-3MB (original image size)
- Content-Type: image/jpeg

---

### Step 4: Verify MongoDB Collections

Check that GridFS collections were created:

```bash
# Connect to your MongoDB
# Check collections
use your_database_name
show collections

# Should see:
# - ads (existing)
# - adImages.files (NEW)
# - adImages.chunks (NEW)

# Count files
db.adImages.files.countDocuments()
# Should return: ~90 (45 ads × 2 images each)

# Check one file
db.adImages.files.findOne()
```

**Expected GridFS File:**
```json
{
  "_id": ObjectId("673abc..."),
  "filename": "672a0a85c1fd5e61889e5f2e_bottom.jpg",
  "length": 1247892,
  "chunkSize": 261120,
  "uploadDate": ISODate("2025-11-05T08:30:00Z"),
  "metadata": {
    "adId": "672a0a85c1fd5e61889e5f2e",
    "type": "bottom",
    "title": "Murtaza Cards"
  }
}
```

---

### Step 5: Cleanup Base64 Data (AFTER VERIFICATION)

⚠️ **ONLY run this after confirming everything works!**

This removes the old base64 data from ad documents to save space:

```bash
npm run migrate:gridfs:cleanup
```

**Expected Output:**
```
🧹 Starting cleanup: Removing base64 data from ads
✅ Connected to MongoDB
✅ Cleaned up 45 ads
💾 Estimated space saved: ~60-70MB per ad
```

**What it does:**
- Sets `bottomImage` and `fullscreenImage` fields to empty strings
- GridFS references remain intact
- Saves ~60-70MB per ad document

---

## 🔍 Troubleshooting

### Migration Fails: "GridFS bucket not initialized"

**Solution:**
- Ensure MongoDB connection is active
- Check MONGODB_URI in .env file
- Restart migration

### Images Return 404

**Symptom:** `/api/ads/image/:id/bottom` returns 404

**Solution:**
1. Check if migration completed successfully
2. Verify `bottomImageGridFS` field exists in ad documents:
   ```bash
   db.ads.findOne({ _id: ObjectId("...") })
   ```
3. Check GridFS files collection:
   ```bash
   db.adImages.files.find({ "metadata.adId": "..." })
   ```

### Still Getting 502 Errors

**Symptom:** `/api/ads/active` still returns 502

**Solution:**
1. Check if you're hitting the old endpoint somehow
2. Clear browser/app cache
3. Verify Render deploy completed
4. Check server logs in Render dashboard

### Migration Shows "0 ads to migrate"

**Already Migrated!** This means:
- All ads already have GridFS references
- Migration was successful
- You can proceed to cleanup step

---

## ✅ Success Checklist

After migration, verify:

- [ ] `/api/ads/active` returns in <0.5s
- [ ] Response size is <100KB (check Network tab)
- [ ] No 502 errors
- [ ] Images accessible at `/api/ads/image/:id/bottom`
- [ ] Images load correctly (download test image)
- [ ] GridFS collections exist (`adImages.files`, `adImages.chunks`)
- [ ] Mobile app can load ads (test after updating hook)

---

## 📱 Mobile App Update (Next)

After successful migration, update the mobile app:

**File:** `InstantllyCards/hooks/useAds.ts`

```typescript
export const useAds = () => {
  return useQuery({
    queryKey: ['footer-ads'],
    queryFn: async () => {
      const response = await api.get('/ads/active');
      const { data, imageBaseUrl } = response.data;
      
      return data.map(ad => ({
        id: `api-${ad._id}`,
        // NEW: Use image URLs from GridFS
        image: { uri: `${imageBaseUrl}${ad.bottomImageUrl}` },
        bannerImage: ad.hasFullscreenImage 
          ? { uri: `${imageBaseUrl}${ad.fullscreenImageUrl}` }
          : undefined,
        phone: ad.phoneNumber,
        name: ad.title,
        hasFullBanner: ad.hasFullscreenImage
      }));
    },
    staleTime: 5 * 60 * 1000,
    gcTime: 30 * 60 * 1000,
    refetchInterval: 10 * 60 * 1000
  });
};
```

---

## 🎉 Expected Results

After migration:

✅ **502 errors eliminated** - No more gateway timeouts
✅ **38x faster responses** - <0.5s instead of 19s
✅ **99.8% smaller payloads** - <100KB instead of 66MB
✅ **Better caching** - Images cached 24 hours
✅ **Scalable** - Can handle 1000+ ads

---

## 🔄 Rollback (If Needed)

If something goes wrong:

1. **GridFS files are preserved** - Migration keeps base64 as backup
2. **Fallback works** - API will serve base64 if GridFS not available
3. **No data loss** - Original images remain until cleanup

To fully rollback:
- Redeploy previous version from Git
- Base64 data still in documents (unless cleanup ran)

---

## 📞 Support

**Migration Issues:**
- Check server logs: Render Dashboard → Logs
- Verify MongoDB connection
- Ensure GridFS initialized (check startup logs)

**Current Status:**
- Code: ✅ Deployed to Render
- Migration: ⏳ Waiting to run
- Testing: ⏳ Pending

---

**Next Action:** Run migration with:
```bash
cd "/Users/muskaan7862407/Desktop/Instantlly app/Instantlly-Cards-Backend"
npm run migrate:gridfs
```

Good luck! 🚀
