# 🔧 Setting Up Local Testing Environment

You need to create a `.env` file to test push notifications locally.

## Step 1: Get MongoDB URI from Render

1. **Go to Render Dashboard:**
   https://dashboard.render.com/

2. **Click on your backend service:**
   - Look for "instantlly-cards-backend" or similar

3. **Click "Environment" tab** (in the left sidebar)

4. **Find and copy `MONGODB_URI`:**
   - It should look like: `mongodb+srv://username:password@cluster.mongodb.net/instantllycards`
   - Click the eye icon to reveal the full value
   - Copy the entire string

## Step 2: Create .env File

1. **Create a new file** in `Instantlly-Cards-Backend` folder named `.env` (no extension)

2. **Add this content:**
   ```
   MONGODB_URI=paste_your_mongodb_uri_here
   ```

3. **Replace** `paste_your_mongodb_uri_here` with the actual MongoDB URI you copied from Render

## Step 3: (Optional) Add Expo Access Token

If you want to use FCM v1 API (recommended), also add:

```
EXPO_ACCESS_TOKEN=your_expo_token
```

**To get Expo Access Token:**
1. Go to https://expo.dev/
2. Log in to your account
3. Go to Account Settings → Access Tokens
4. Create a new token
5. Copy and paste it

**Note:** This is optional. The script will work without it using the legacy push API.

## Step 4: Run the Test

```bash
node test-manual-notification.js farhangori89@gmail.com
```

---

## Quick PowerShell Commands

### Create .env file (Windows):
```powershell
# Navigate to backend folder
cd C:\Users\user3\Documents\App\Instantlly-Cards-Backend

# Create .env file
New-Item -Path .env -ItemType File

# Open in notepad
notepad .env
```

Then paste:
```
MONGODB_URI=mongodb+srv://your_uri_here
```

Save and close.

---

## Example .env File

```env
MONGODB_URI=mongodb+srv://farhan:mypassword123@cluster0.abcde.mongodb.net/instantllycards?retryWrites=true&w=majority
EXPO_ACCESS_TOKEN=abc123xyz789
PORT=3000
```

---

## Troubleshooting

**"Cannot find .env file"**
- Make sure the file is named exactly `.env` (not `.env.txt`)
- It should be in the `Instantlly-Cards-Backend` folder
- On Windows, turn on "Show file extensions" to verify

**"Connection timeout"**
- Check if MongoDB URI is correct
- Verify your IP is whitelisted in MongoDB Atlas
- Try adding `&retryWrites=true&w=majority` to the end of the URI

**"Authentication failed"**
- Check username and password in MongoDB URI
- Make sure password is URL-encoded (no special characters)

---

## Security Note

⚠️ **NEVER commit the `.env` file to Git!**

The `.gitignore` should already have `.env` listed. Verify:

```bash
cat .gitignore | grep .env
```

Should show:
```
.env
```

---

**Once you've created the `.env` file, run the test again!** 🚀
