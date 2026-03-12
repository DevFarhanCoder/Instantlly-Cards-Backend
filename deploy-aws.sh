#!/bin/bash
# Deploy to both AWS servers (production + staging)
# Usage: ./deploy-aws.sh

set -e

PROD_IP="3.90.205.216"
STAGING_IP="3.235.244.10"
PROD_KEY="/tmp/production_key.pem"
STAGING_KEY="/tmp/staging_key.pem"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PPK_PROD="$SCRIPT_DIR/production_instant_key.ppk"
PPK_STAGING="$SCRIPT_DIR/staging_mongo_key.ppk"

echo "🚀 Starting AWS Deployment"
echo "================================"

# Step 1: Convert .ppk to PEM if needed
echo "🔑 Converting keys..."
puttygen "$PPK_PROD" -O private-openssh -o "$PROD_KEY" 2>/dev/null
chmod 400 "$PROD_KEY"
puttygen "$PPK_STAGING" -O private-openssh -o "$STAGING_KEY" 2>/dev/null
chmod 400 "$STAGING_KEY"
echo "✅ Keys ready"

# Step 2: Build dist locally (avoids out-of-memory on servers)
echo ""
echo "🔨 Building TypeScript locally..."
cd "$SCRIPT_DIR"
npm run build
echo "✅ Build complete"

# Step 3: Deploy to PRODUCTION server
echo ""
echo "🌐 Deploying to PRODUCTION (api.instantllycards.com)..."
ssh -i "$PROD_KEY" -o StrictHostKeyChecking=no ubuntu@$PROD_IP "
  cd ~/Instantlly-Cards-Backend
  git fetch origin
  git reset --hard origin/main
  echo 'Git updated'
"

# Copy compiled dist to production
echo "📦 Copying dist/ to production..."
scp -i "$PROD_KEY" -o StrictHostKeyChecking=no -r "$SCRIPT_DIR/dist" ubuntu@$PROD_IP:~/Instantlly-Cards-Backend/

# Restart production
ssh -i "$PROD_KEY" -o StrictHostKeyChecking=no ubuntu@$PROD_IP "cd ~/Instantlly-Cards-Backend && pm2 restart all && sleep 2 && pm2 list"
echo "✅ Production deployed & restarted"

# Step 4: Deploy to STAGING server
echo ""
echo "🧪 Deploying to STAGING (api-test.instantllycards.com)..."
ssh -i "$STAGING_KEY" -o StrictHostKeyChecking=no ubuntu@$STAGING_IP "
  cd ~/Inc-backend-testing
  git fetch origin
  git reset --hard origin/main
  echo 'Git updated'
"

# Copy compiled dist to staging
echo "📦 Copying dist/ to staging..."
scp -i "$STAGING_KEY" -o StrictHostKeyChecking=no -r "$SCRIPT_DIR/dist" ubuntu@$STAGING_IP:~/Inc-backend-testing/

# Restart staging
ssh -i "$STAGING_KEY" -o StrictHostKeyChecking=no ubuntu@$STAGING_IP "pm2 restart all && sleep 2 && pm2 list"
echo "✅ Staging deployed & restarted"

# Step 5: Verify CORS
echo ""
echo "🔍 Verifying CORS on both servers..."
PROD_STATUS=$(curl -s -o /dev/null -w "%{http_code}" -X OPTIONS https://api.instantllycards.com/api/auth/login \
  -H "Origin: https://instantllychannelpatner.vercel.app" \
  -H "Access-Control-Request-Method: POST")

STAGING_STATUS=$(curl -s -o /dev/null -w "%{http_code}" -X OPTIONS https://api-test.instantllycards.com/api/auth/login \
  -H "Origin: https://instantllychannelpatner.vercel.app" \
  -H "Access-Control-Request-Method: POST")

echo "  Production CORS: $PROD_STATUS (expected 204)"
echo "  Staging CORS:    $STAGING_STATUS (expected 204)"

echo ""
echo "================================"
echo "🎉 Deployment Complete!"
