# PowerShell Profile Setup Script
# This script will install and configure your PowerShell profile with bash-like settings

param(
    [switch]$Force,
    [switch]$InstallModules
)

Write-Host "Setting up PowerShell profile with bash-like configuration..." -ForegroundColor Green

# Check PowerShell version
if ($PSVersionTable.PSVersion.Major -lt 5) {
    Write-Error "PowerShell 5.0 or higher is required"
    exit 1
}

# Get profile path
$ProfilePath = $PROFILE.CurrentUserCurrentHost
$ProfileDir = Split-Path $ProfilePath

Write-Host "Profile will be installed to: $ProfilePath" -ForegroundColor Cyan

# Create profile directory if it doesn't exist
if (!(Test-Path $ProfileDir)) {
    Write-Host "Creating profile directory..." -ForegroundColor Yellow
    New-Item -ItemType Directory -Path $ProfileDir -Force | Out-Null
}

# Backup existing profile if it exists
if (Test-Path $ProfilePath) {
    if (!$Force) {
        $response = Read-Host "Profile already exists. Backup and replace? (y/N)"
        if ($response -ne 'y' -and $response -ne 'Y') {
            Write-Host "Setup cancelled." -ForegroundColor Yellow
            exit 0
        }
    }

    $BackupPath = "$ProfilePath.backup.$(Get-Date -Format 'yyyyMMdd-HHmmss')"
    Write-Host "Backing up existing profile to: $BackupPath" -ForegroundColor Yellow
    Copy-Item $ProfilePath $BackupPath
}

# Create symlink to the profile (or copy if symlink fails)
Write-Host "Installing new PowerShell profile..." -ForegroundColor Green
$SourcePath = Resolve-Path "Microsoft.PowerShell_profile.ps1"

try {
    # Try to create a symbolic link
    New-Item -ItemType SymbolicLink -Path $ProfilePath -Target $SourcePath -Force
    Write-Host "✓ Created symbolic link to profile" -ForegroundColor Green
} catch {
    # Fall back to copying if symlink creation fails
    Write-Warning "Could not create symbolic link (may need admin rights). Copying file instead."
    Copy-Item "Microsoft.PowerShell_profile.ps1" $ProfilePath -Force
    Write-Host "✓ Copied profile file" -ForegroundColor Yellow
}

# Install recommended modules
if ($InstallModules) {
    Write-Host "Installing recommended PowerShell modules..." -ForegroundColor Green

    $ModulesToInstall = @(
        'PSReadLine',
        'posh-git',
        'Terminal-Icons'
    )
    foreach ($Module in $ModulesToInstall) {
        Write-Host "Installing $Module..." -ForegroundColor Cyan
        try {
            Install-Module -Name $Module -Scope CurrentUser -Force -AllowClobber
            Write-Host "✓ $Module installed successfully" -ForegroundColor Green
        } catch {
            Write-Warning "Failed to install ${Module}: $($_.Exception.Message)"
        }
    }
}

Write-Host "`nSetup complete! 🎉" -ForegroundColor Green
Write-Host "`nTo apply the changes:" -ForegroundColor Cyan
Write-Host "1. Restart PowerShell, or" -ForegroundColor White
Write-Host "2. Run: . `$PROFILE" -ForegroundColor White

Write-Host "`nRecommended next steps:" -ForegroundColor Cyan
Write-Host "• Install modern CLI tools:" -ForegroundColor White
Write-Host "  - bat (better cat): winget install sharkdp.bat" -ForegroundColor Gray
Write-Host "  - fd (better find): winget install sharkdp.fd" -ForegroundColor Gray
Write-Host "  - ripgrep (better grep): winget install BurntSushi.ripgrep.MSVC" -ForegroundColor Gray
Write-Host "  - fzf (fuzzy finder): winget install junegunn.fzf" -ForegroundColor Gray
Write-Host "• Install a Nerd Font for better terminal icons" -ForegroundColor White
Write-Host "• Consider Windows Terminal for the best experience" -ForegroundColor White

if (!$InstallModules) {
    Write-Host "`nTo install recommended PowerShell modules, run:" -ForegroundColor Yellow
    Write-Host ".\setup-powershell-profile.ps1 -InstallModules" -ForegroundColor White
}
