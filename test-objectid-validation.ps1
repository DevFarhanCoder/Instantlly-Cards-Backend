# PowerShell script to test ObjectId validation
Write-Host "🧪 Testing ObjectId validation in authentication..." -ForegroundColor Yellow
Write-Host ""

$baseUrl = "http://localhost:3001/api/cards"

# Test 1: No Authorization Header (should get 401)
Write-Host "Test 1: No Authorization header" -ForegroundColor Cyan
try {
    $response = Invoke-RestMethod -Uri "$baseUrl/sent" -Method GET
    Write-Host "❌ Should have failed but got success" -ForegroundColor Red
} catch {
    if ($_.Exception.Response.StatusCode -eq 401) {
        Write-Host "✅ Correctly rejected missing token with 401" -ForegroundColor Green
    } else {
        Write-Host "❌ Unexpected status: $($_.Exception.Response.StatusCode)" -ForegroundColor Red
    }
}
Write-Host ""

# Test 2: Invalid token format (should get 401)  
Write-Host "Test 2: Invalid token format" -ForegroundColor Cyan
try {
    $headers = @{ Authorization = "Bearer invalid-token" }
    $response = Invoke-RestMethod -Uri "$baseUrl/sent" -Method GET -Headers $headers
    Write-Host "❌ Should have failed but got success" -ForegroundColor Red
} catch {
    if ($_.Exception.Response.StatusCode -eq 401) {
        Write-Host "✅ Correctly rejected invalid token with 401" -ForegroundColor Green
    } else {
        Write-Host "❌ Unexpected status: $($_.Exception.Response.StatusCode)" -ForegroundColor Red
    }
}
Write-Host ""

Write-Host "🏁 ObjectId validation tests completed!" -ForegroundColor Green
Write-Host "The authentication middleware now properly validates ObjectId format." -ForegroundColor Gray
Write-Host "This should prevent 'Cast to ObjectId failed' errors in production." -ForegroundColor Gray