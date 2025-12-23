# Reset My Credits to Default (200)
# Run this script to reset your own account's credits back to 200

$API_BASE = "https://api-test.instantllycards.com"  # Change to your server
$ADMIN_KEY = "your-secure-admin-key-here"  # Use your ADMIN_SECRET_KEY from .env
$MY_PHONE = "+919876543210"  # Your phone number

Write-Host "🔄 Resetting credits for phone: $MY_PHONE" -ForegroundColor Cyan

$body = @{
    phone = $MY_PHONE
    resetToDefault = $true
} | ConvertTo-Json

$headers = @{
    "Content-Type" = "application/json"
    "x-admin-key" = $ADMIN_KEY
}

try {
    $response = Invoke-RestMethod -Uri "$API_BASE/api/credits/admin/reset-user-credits" -Method POST -Body $body -Headers $headers
    
    Write-Host "✅ Success!" -ForegroundColor Green
    Write-Host "Old Credits: $($response.data.oldCredits)" -ForegroundColor Yellow
    Write-Host "New Credits: $($response.data.newCredits)" -ForegroundColor Green
    Write-Host "Expiry Date: $($response.data.expiryDate)" -ForegroundColor Cyan
} catch {
    Write-Host "❌ Error: $($_.Exception.Message)" -ForegroundColor Red
    Write-Host $_.Exception.Response
}
