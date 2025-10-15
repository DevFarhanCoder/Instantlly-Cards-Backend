# Manual Dependency Installation Script
# Use this if rebar3 compile is too slow

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Manual Dependency Installation" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# Create _build/default/lib directory
$libDir = "_build\default\lib"
New-Item -ItemType Directory -Path $libDir -Force | Out-Null

# Download dependencies directly from GitHub
$dependencies = @(
    @{
        Name = "cowboy"
        Url = "https://github.com/ninenines/cowboy/archive/refs/tags/2.12.0.zip"
        File = "cowboy.zip"
    },
    @{
        Name = "cowlib"
        Url = "https://github.com/ninenines/cowlib/archive/refs/tags/2.13.0.zip"
        File = "cowlib.zip"
    },
    @{
        Name = "ranch"
        Url = "https://github.com/ninenines/ranch/archive/refs/tags/1.8.0.zip"
        File = "ranch.zip"
    },
    @{
        Name = "jsx"
        Url = "https://github.com/talentdeficit/jsx/archive/refs/tags/v3.1.0.zip"
        File = "jsx.zip"
    }
)

foreach ($dep in $dependencies) {
    Write-Host "📦 Downloading $($dep.Name)..." -ForegroundColor Yellow
    
    $zipPath = Join-Path $libDir $dep.File
    $extractPath = Join-Path $libDir $dep.Name
    
    try {
        # Download
        Invoke-WebRequest -Uri $dep.Url -OutFile $zipPath -UseBasicParsing
        Write-Host "  ✅ Downloaded" -ForegroundColor Green
        
        # Extract
        Expand-Archive -Path $zipPath -DestinationPath $libDir -Force
        
        # Rename folder (GitHub adds version suffix)
        $extracted = Get-ChildItem -Path $libDir -Filter "$($dep.Name)-*" -Directory | Select-Object -First 1
        if ($extracted) {
            Move-Item -Path $extracted.FullName -Destination $extractPath -Force
        }
        
        # Clean up zip
        Remove-Item $zipPath -Force
        
        Write-Host "  ✅ Installed" -ForegroundColor Green
    } catch {
        Write-Host "  ❌ Failed: $($_.Exception.Message)" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "✅ Dependencies installed!" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Now run: escript rebar3 compile" -ForegroundColor Yellow
