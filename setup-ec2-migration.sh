#!/bin/bash
# EC2 Migration Setup Script
# Run this ON YOUR EC2 INSTANCE after connecting

echo "🚀 Setting up Migration Environment on EC2"
echo "=========================================="

# Step 1: Install Node.js
echo "📦 Installing Node.js..."
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
nvm install 18
nvm use 18

echo "✅ Node.js installed:"
node --version
npm --version

# Step 2: Install dependencies
echo ""
echo "📦 Installing dependencies..."
npm install mongoose dotenv

echo ""
echo "✅ Setup complete!"
echo ""
echo "📝 Next steps:"
echo "1. Ensure .env file has correct credentials"
echo "2. Run: node migrate-to-documentdb.js"
echo "3. Verify: node verify-migration.js"
