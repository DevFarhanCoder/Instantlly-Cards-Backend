#!/bin/bash
# Deployment script for AWS EC2
# Run this on the server: bash deploy.sh

echo "🚀 Starting deployment..."
cd ~/Instantlly-Cards-Backend

echo "📥 Pulling latest code from GitHub..."
git pull origin main

echo "📦 Installing dependencies..."
npm install

echo "🔨 Building TypeScript code..."
npm run build

echo "🔄 Restarting PM2..."
pm2 restart all

echo "📊 Checking PM2 status..."
pm2 status

echo "📝 Showing last 20 log lines..."
pm2 logs --lines 20 --nostream

echo "✅ Deployment complete!"
echo "🌐 Backend should be live at: https://api.instantllycards.com"
