# Erlang Message Gateway - Windows Setup Script
# Run this script to install all dependencies

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Erlang Message Gateway - Setup" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# Check if running as Administrator
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if (-not $isAdmin) {
    Write-Host "⚠️  This script requires Administrator privileges" -ForegroundColor Yellow
    Write-Host "Please run PowerShell as Administrator and try again" -ForegroundColor Yellow
    exit 1
}

# Function to check if command exists
function Test-Command {
    param($Command)
    try {
        Get-Command $Command -ErrorAction Stop
        return $true
    } catch {
        return $false
    }
}

# 1. Check/Install Chocolatey
Write-Host "📦 Checking Chocolatey..." -ForegroundColor Yellow
if (-not (Test-Command choco)) {
    Write-Host "Installing Chocolatey..." -ForegroundColor Green
    Set-ExecutionPolicy Bypass -Scope Process -Force
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
    Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
    refreshenv
} else {
    Write-Host "✅ Chocolatey already installed" -ForegroundColor Green
}

# 2. Check/Install Erlang
Write-Host ""
Write-Host "🔧 Checking Erlang/OTP..." -ForegroundColor Yellow
if (-not (Test-Command erl)) {
    Write-Host "Installing Erlang/OTP 26..." -ForegroundColor Green
    choco install erlang -y
    refreshenv
} else {
    Write-Host "✅ Erlang already installed" -ForegroundColor Green
    erl -version
}

# 3. Download Rebar3 (if not exists)
Write-Host ""
Write-Host "🛠️  Checking Rebar3..." -ForegroundColor Yellow
if (-not (Test-Path ".\rebar3")) {
    Write-Host "Downloading Rebar3..." -ForegroundColor Green
    try {
        Invoke-WebRequest -Uri "https://s3.amazonaws.com/rebar3/rebar3" -OutFile ".\rebar3" -UseBasicParsing
        Write-Host "✅ Rebar3 downloaded" -ForegroundColor Green
    } catch {
        Write-Host "❌ Failed to download Rebar3. Please download manually from:" -ForegroundColor Red
        Write-Host "https://github.com/erlang/rebar3/releases" -ForegroundColor Yellow
    }
} else {
    Write-Host "✅ Rebar3 already exists" -ForegroundColor Green
}

# 4. Optional: PostgreSQL
Write-Host ""
Write-Host "🗄️  PostgreSQL (Optional for Phase 2)..." -ForegroundColor Yellow
if (-not (Test-Command psql)) {
    $installPostgres = Read-Host "Install PostgreSQL? (y/n)"
    if ($installPostgres -eq 'y') {
        choco install postgresql15 -y
        refreshenv
    }
} else {
    Write-Host "✅ PostgreSQL already installed" -ForegroundColor Green
}

# 5. Optional: Redis
Write-Host ""
Write-Host "📊 Redis (Optional for Phase 2)..." -ForegroundColor Yellow
if (-not (Test-Command redis-cli)) {
    $installRedis = Read-Host "Install Redis? (y/n)"
    if ($installRedis -eq 'y') {
        choco install redis-64 -y
        refreshenv
    }
} else {
    Write-Host "✅ Redis already installed" -ForegroundColor Green
}

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "✅ Setup Complete!" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "1. Close and reopen PowerShell (to refresh environment)" -ForegroundColor White
Write-Host "2. Run: .\rebar3 compile" -ForegroundColor White
Write-Host "3. Run: .\rebar3 shell" -ForegroundColor White
Write-Host "4. Open http://localhost:8080 in your browser" -ForegroundColor White
Write-Host ""
