# Build and Run WITHOUT Dependencies
# This compiles a simple test server immediately

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Building Simple Test Server (No Deps)" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

$erlc = "C:\Program Files\Erlang OTP\bin\erlc.exe"
$erl = "C:\Program Files\Erlang OTP\bin\erl.exe"

# Create ebin directory
New-Item -ItemType Directory -Path "ebin" -Force | Out-Null

Write-Host "📦 Compiling modules..." -ForegroundColor Yellow

# Compile simple modules
$modules = @(
    "src/simple_test.erl",
    "src/standalone_server.erl"
)

foreach ($module in $modules) {
    Write-Host "  Compiling $module..." -ForegroundColor Gray
    &$erlc -o ebin $module
    if ($LASTEXITCODE -eq 0) {
        Write-Host "  ✅ OK" -ForegroundColor Green
    } else {
        Write-Host "  ❌ Failed" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "✅ Build Complete!" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Starting Erlang shell..." -ForegroundColor Yellow
Write-Host ""
Write-Host "Commands you can try:" -ForegroundColor White
Write-Host '  simple_test:start().' -ForegroundColor Cyan
Write-Host '  simple_test:hello().' -ForegroundColor Cyan
Write-Host '  standalone_server:start().' -ForegroundColor Cyan
Write-Host '  q().' -ForegroundColor Gray
Write-Host ""

# Start Erlang with our modules
&$erl -pa ebin
