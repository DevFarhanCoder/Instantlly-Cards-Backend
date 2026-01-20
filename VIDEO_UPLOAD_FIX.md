# Video Upload Fix - HTTP 413 Error

## Problem
Error: "SyntaxError: JSON Parse error: Unexpected character: <"
Status: 413 (Payload Too Large)

This happens when video files exceed the server's upload limit.

---

## Solutions Applied

### 1. Backend Changes ✅
- Increased multer video limit: **50MB → 100MB**
- Increased express body limit: **20MB → 150MB**
- Added better error handling in mobile app

### 2. Platform-Specific Configuration

#### **AWS EC2 / VPS with Nginx**
Add to `/etc/nginx/nginx.conf` or `/etc/nginx/conf.d/upload.conf`:
```nginx
client_max_body_size 150M;
client_body_timeout 300s;
proxy_read_timeout 300s;
```

Then restart nginx:
```bash
sudo nginx -t
sudo systemctl restart nginx
```

#### **Render.com**
Add to `render.yaml`:
```yaml
services:
  - type: web
    name: instantlly-backend
    env: node
    buildCommand: npm install && npm run build
    startCommand: npm start
    envVars:
      - key: NODE_OPTIONS
        value: --max-old-space-size=2048
    # Render automatically handles large uploads
```

#### **AWS Elastic Beanstalk**
Create `.ebextensions/nginx.config`:
```yaml
files:
  "/etc/nginx/conf.d/proxy.conf":
    mode: "000644"
    owner: root
    group: root
    content: |
      client_max_body_size 150M;
      client_body_timeout 300s;
```

#### **Heroku**
No configuration needed - Heroku handles this automatically.

---

## Mobile App Changes ✅

Added better error handling:
- Detects 413 errors
- Shows user-friendly message
- Suggests video compression

---

## Testing Steps

1. **Restart your backend server**
   ```bash
   cd Instantlly-Cards-Backend
   npm run dev
   ```

2. **Clear mobile app cache**
   - Close and restart the app
   - Try uploading a video

3. **Test with different video sizes**
   - Small video (< 10MB) ✓
   - Medium video (10-50MB) ✓
   - Large video (50-80MB) ✓

---

## Recommended Video Specifications

For best performance:
- **Format**: MP4 (H.264)
- **Max Size**: 80MB
- **Resolution**: 1080p or lower
- **Duration**: Under 30 seconds
- **Bitrate**: 2-5 Mbps

---

## User Instructions

Tell users to:
1. Compress videos before uploading
2. Use video compressor apps (VidCompact, Video Compressor, etc.)
3. Keep videos under 80MB
4. Reduce quality if needed

---

## Troubleshooting

### Still getting 413 error?

1. **Check your deployment platform**
   - AWS: Update nginx config
   - Render: No action needed
   - Heroku: Should work automatically

2. **Restart everything**
   ```bash
   # Backend
   pm2 restart all
   # Or
   sudo systemctl restart nginx
   ```

3. **Check logs**
   ```bash
   # Backend logs
   pm2 logs
   # Nginx logs
   tail -f /var/log/nginx/error.log
   ```

### Video too large even after fix?

Ask user to:
- Use lower resolution (720p instead of 1080p)
- Reduce video duration
- Use video compression app
- Split into multiple shorter videos

---

## Current Limits

| Type | Old Limit | New Limit |
|------|-----------|-----------|
| Video File | 50MB | **100MB** |
| Body Parser | 20MB | **150MB** |
| Recommended | - | **< 80MB** |

---

## Next Steps

1. Deploy backend changes to production
2. Configure nginx on your server
3. Test video uploads
4. Monitor server memory usage
5. Consider video compression on server side if needed
