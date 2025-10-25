# Quick Start Script for Erlang Message Gateway

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🚀 Starting Erlang Message Gateway" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# Check if compiled
if (-not (Test-Path ".\_build\default\lib\message_gateway")) {
    Write-Host "⚙️  Compiling project..." -ForegroundColor Yellow
    & .\rebar3 compile
    if ($LASTEXITCODE -ne 0) {
        Write-Host "❌ Compilation failed!" -ForegroundColor Red
        Write-Host "Please check the errors above" -ForegroundColor Yellow
        exit 1
    }
}

Write-Host "✅ Starting server..." -ForegroundColor Green
Write-Host ""
Write-Host "📍 WebSocket: ws://localhost:8080/ws" -ForegroundColor White
Write-Host "📍 Web Client: http://localhost:8080" -ForegroundColor White
Write-Host "📍 Health Check: http://localhost:8080/health" -ForegroundColor White
Write-Host ""
Write-Host "Press Ctrl+C twice to stop" -ForegroundColor Yellow
Write-Host ""

# Start Erlang shell
& .\rebar3 shell
