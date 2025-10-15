# Simple build script without rebar3
# This compiles all Erlang files directly

Write-Host "===========================================" -ForegroundColor Cyan
Write-Host "Compiling Erlang Files..." -ForegroundColor Cyan
Write-Host "===========================================" -ForegroundColor Cyan
Write-Host ""

$erlc = "C:\Program Files\Erlang OTP\bin\erlc.exe"
$erl = "C:\Program Files\Erlang OTP\bin\erl.exe"

# Create ebin directory
if (-not (Test-Path "ebin")) {
    New-Item -ItemType Directory -Path "ebin" | Out-Null
}

# Compile simple test first
Write-Host "Compiling simple_test.erl..." -ForegroundColor Yellow
&$erlc -o ebin src/simple_test.erl

if ($LASTEXITCODE -eq 0) {
    Write-Host "✅ Compilation successful!" -ForegroundColor Green
    Write-Host ""
    Write-Host "Testing..." -ForegroundColor Yellow
    Write-Host ""
    
    # Run the test
    &$erl -pa ebin -noshell -s simple_test start -s simple_test hello -s init stop
    
    Write-Host ""
    Write-Host "===========================================" -ForegroundColor Cyan
    Write-Host "✅ Erlang is working correctly!" -ForegroundColor Green
    Write-Host "===========================================" -ForegroundColor Cyan
} else {
    Write-Host "❌ Compilation failed" -ForegroundColor Red
}
