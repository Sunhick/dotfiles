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
    "function.ps1"
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

# Load work configuration if present
$WorkEntryPoint = Join-Path $ProfileDir "work\work.ps1"
if (Test-Path $WorkEntryPoint) {
    try {
        . $WorkEntryPoint
        Write-Verbose "Loaded: work profile"
    } catch {
        Write-Warning "Failed to load work profile: $($_.Exception.Message)"
    }
}

#region Welcome Message
Write-Host "Personal profile loaded." -ForegroundColor Green
if (Test-Path $WorkEntryPoint) {
    Write-Host "Work profile loaded." -ForegroundColor Green
}
#endregion
