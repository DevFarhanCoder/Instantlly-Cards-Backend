# Simple Build Script - No Special Characters

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Building Test Server" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

$erlc = "C:\Program Files\Erlang OTP\bin\erlc.exe"
$erl = "C:\Program Files\Erlang OTP\bin\erl.exe"

# Create ebin directory
New-Item -ItemType Directory -Path "ebin" -Force | Out-Null

Write-Host "Compiling modules..." -ForegroundColor Yellow

# Compile simple modules
$modules = @(
    "src/simple_test.erl",
    "src/standalone_server.erl"
)

foreach ($module in $modules) {
    Write-Host "  Compiling $module..." -ForegroundColor Gray
    &$erlc -o ebin $module
    if ($LASTEXITCODE -eq 0) {
        Write-Host "  OK" -ForegroundColor Green
    } else {
        Write-Host "  Failed" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Build Complete!" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Starting Erlang shell..." -ForegroundColor Yellow
Write-Host ""
Write-Host "Commands to try:" -ForegroundColor White
Write-Host "  simple_test:start(). " -ForegroundColor Cyan
Write-Host "  simple_test:hello(). " -ForegroundColor Cyan
Write-Host "  q(). to quit" -ForegroundColor Gray
Write-Host ""

# Start Erlang with our modules
&$erl -pa ebin
