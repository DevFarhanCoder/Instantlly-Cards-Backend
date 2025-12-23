# Migrate All Existing Users to New Credit System
# Run this ONCE after deploying the new credit system

$API_BASE = "https://api-test.instantllycards.com"  # Your production API
$ADMIN_KEY = "your-secure-admin-key-here"  # Use your ADMIN_SECRET_KEY from .env

Write-Host "🚀 CREDIT MIGRATION - All Users" -ForegroundColor Cyan
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "⚠️  WARNING: This will reset ALL users' credits to 200!" -ForegroundColor Yellow
Write-Host ""
Write-Host "This will:" -ForegroundColor White
Write-Host "  - Find all users with credits ≠ 200" -ForegroundColor White
Write-Host "  - Reset their credits to 200" -ForegroundColor White
Write-Host "  - Set new expiry date (30 days from now)" -ForegroundColor White
Write-Host ""
$confirm = Read-Host "Type 'MIGRATE' to proceed"

if ($confirm -ne "MIGRATE") {
    Write-Host "❌ Migration cancelled" -ForegroundColor Red
    exit
}

Write-Host ""
Write-Host "🔄 Starting migration..." -ForegroundColor Cyan

$headers = @{
    "Content-Type" = "application/json"
    "x-admin-key" = $ADMIN_KEY
}

try {
    $response = Invoke-RestMethod -Uri "$API_BASE/api/credits/admin/migrate-all-users" -Method POST -Headers $headers
    
    Write-Host ""
    Write-Host "✅ MIGRATION SUCCESSFUL!" -ForegroundColor Green
    Write-Host "========================" -ForegroundColor Green
    Write-Host ""
    Write-Host "📊 Statistics:" -ForegroundColor Cyan
    Write-Host "  Total Users: $($response.stats.totalUsers)" -ForegroundColor White
    Write-Host "  Migrated: $($response.stats.migratedUsers)" -ForegroundColor Green
    Write-Host "  Target Credits: $($response.stats.targetCredits)" -ForegroundColor Yellow
    Write-Host "  New Expiry: $($response.stats.expiryDate)" -ForegroundColor Cyan
    Write-Host ""
    
    if ($response.sample -and $response.sample.Count -gt 0) {
        Write-Host "📝 Sample Migrations (first 10):" -ForegroundColor Cyan
        foreach ($user in $response.sample) {
            Write-Host "  - $($user.name) ($($user.phone)): $($user.oldCredits) → $($user.newCredits)" -ForegroundColor White
        }
    }
    
    Write-Host ""
    Write-Host "✅ All existing users now have 200 credits!" -ForegroundColor Green
    
} catch {
    Write-Host ""
    Write-Host "❌ MIGRATION FAILED" -ForegroundColor Red
    Write-Host "===================" -ForegroundColor Red
    Write-Host ""
    Write-Host "Error: $($_.Exception.Message)" -ForegroundColor Red
    
    if ($_.Exception.Response) {
        $reader = New-Object System.IO.StreamReader($_.Exception.Response.GetResponseStream())
        $responseBody = $reader.ReadToEnd()
        Write-Host "Response: $responseBody" -ForegroundColor Yellow
    }
    
    Write-Host ""
    Write-Host "💡 Troubleshooting:" -ForegroundColor Cyan
    Write-Host "  1. Check ADMIN_SECRET_KEY is correct" -ForegroundColor White
    Write-Host "  2. Verify backend is deployed and running" -ForegroundColor White
    Write-Host "  3. Check API_BASE URL" -ForegroundColor White
}
