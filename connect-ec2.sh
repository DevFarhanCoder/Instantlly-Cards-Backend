#!/bin/bash
# Connect to EC2 and Transfer Migration Files

EC2_IP="34.205.73.94"
KEY_FILE="/Users/muskaan7862407/Desktop/Instantlly app/instantlly.pem"
BACKEND_DIR="/Users/muskaan7862407/Desktop/Instantlly app/Instantlly-Cards-Backend"

echo "🔐 EC2 Migration Helper"
echo "======================="
echo ""
echo "EC2 IP: $EC2_IP"
echo "Key File: $KEY_FILE"
echo ""

# Test connection
echo "📡 Testing SSH connection..."
ssh -i "$KEY_FILE" -o ConnectTimeout=10 -o StrictHostKeyChecking=no ec2-user@$EC2_IP "echo '✅ Connection successful!'" 2>&1

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ SSH connection works!"
    echo ""
    echo "📦 Transferring migration files..."
    
    # Transfer files
    scp -i "$KEY_FILE" \
        "$BACKEND_DIR/migrate-to-documentdb.js" \
        "$BACKEND_DIR/verify-migration.js" \
        "$BACKEND_DIR/global-bundle.pem" \
        "$BACKEND_DIR/.env" \
        "$BACKEND_DIR/package.json" \
        ec2-user@$EC2_IP:~/
    
    if [ $? -eq 0 ]; then
        echo ""
        echo "✅ Files transferred successfully!"
        echo ""
        echo "🚀 Now connecting to EC2 to run migration..."
        echo ""
        
        # Connect to EC2
        ssh -i "$KEY_FILE" ec2-user@$EC2_IP
    else
        echo "❌ File transfer failed"
    fi
else
    echo ""
    echo "❌ Cannot connect to EC2"
    echo ""
    echo "Please check:"
    echo "1. Security group allows SSH (port 22) from your IP"
    echo "2. EC2 instance is running"
    echo "3. Using correct IP address: $EC2_IP"
    echo ""
    echo "To fix security group:"
    echo "1. Go to AWS Console → EC2 → Security Groups"
    echo "2. Select 'instantlly' security group"
    echo "3. Edit Inbound Rules"
    echo "4. Add: SSH (port 22) from My IP or 0.0.0.0/0"
    echo "5. Save rules and wait 1 minute"
fi
