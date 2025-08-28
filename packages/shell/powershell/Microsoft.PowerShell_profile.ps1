# PowerShell Profile - Modular Configuration
# Place this file in your PowerShell profile directory
# Run: $PROFILE to see the expected location

# Get the directory where this profile is located
$ProfileDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ConfigDir = Join-Path $ProfileDir ".config\powershell"

# Source modular configuration files
$ConfigFiles = @(
    "environment.ps1"
    "prompt.ps1"
    "aliases.ps1"
    "history.ps1"
    "fzf.ps1"
)

foreach ($ConfigFile in $ConfigFiles) {
    $ConfigPath = Join-Path $ConfigDir $ConfigFile
    if (Test-Path $ConfigPath) {
        try {
            . $ConfigPath
            Write-Verbose "Loaded: $ConfigFile"
        } catch {
            Write-Warning "Failed to load $ConfigFile`: $($_.Exception.Message)"
        }
    } else {
        Write-Verbose "Config file not found: $ConfigPath"
    }
}

#region Welcome Message
Write-Host "PowerShell profile loaded successfully!" -ForegroundColor Green
Write-Host "Modular configuration loaded from .config/powershell/" -ForegroundColor Cyan
#endregion
