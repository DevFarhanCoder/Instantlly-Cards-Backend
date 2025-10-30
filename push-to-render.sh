#!/bin/bash

echo "🚀 Pushing Enhanced Backend to Render with Fresh Logging"
echo "=================================================="

# You need to replace this with your actual Render repository URL
# Get this from your Render dashboard -> Settings -> Repository
RENDER_REPO_URL="YOUR_RENDER_REPOSITORY_URL_HERE"

echo "📋 Instructions to get your Render repository URL:"
echo "1. Go to https://dashboard.render.com"
echo "2. Click on your backend service"
echo "3. Go to Settings tab"
echo "4. Look for 'Repository' section"
echo "5. Copy the Git repository URL"
echo ""

# Check if user has set the repository URL
if [ "$RENDER_REPO_URL" = "YOUR_RENDER_REPOSITORY_URL_HERE" ]; then
    echo "❌ You need to update the RENDER_REPO_URL in this script first!"
    echo ""
    echo "Please:"
    echo "1. Open this file: push-to-render.sh"
    echo "2. Replace YOUR_RENDER_REPOSITORY_URL_HERE with your actual Render repository URL"
    echo "3. Then run this script again"
    echo ""
    echo "Your repository URL should look like:"
    echo "https://github.com/yourusername/your-repo-name.git"
    echo "or"
    echo "git@github.com:yourusername/your-repo-name.git"
    exit 1
fi

echo "🔗 Adding Render repository as origin..."
git remote add origin $RENDER_REPO_URL

echo "📤 Pushing to main branch..."
git push -u origin main

echo ""
echo "✅ Successfully pushed enhanced backend with logging!"
echo ""
echo "🔍 Now you can monitor fresh logs:"
echo "1. Go to your Render dashboard"
echo "2. Click on your backend service"
echo "3. Go to 'Logs' tab"
echo "4. You should see fresh deployment logs"
echo ""
echo "📱 Test with mobile app:"
echo "1. Open the mobile app"
echo "2. Login with: +918828188930 / 123456"
echo "3. Watch the Render logs for detailed request tracking"
echo ""
echo "🐛 Look for these log patterns:"
echo "   🌐 [REQUEST] - Every API call"
echo "   🔐 [AUTH CHECK] - Authentication attempts"
echo "   📱 [CONTACT FEED DEBUG] - When app requests card data"
echo "   ✅ [AUTH SUCCESS] - Successful authentication"
echo "   👤 User Info: Muskaan (+918828188930)"
echo "   📞 Found X contacts who are app users"
echo "   📇 Found X total cards"
