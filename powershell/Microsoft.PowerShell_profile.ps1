# PowerShell Profile - Modular Configuration
# Place this file in your PowerShell profile directory (run `$PROFILE` to see it).

$ProfileDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ConfigDir = Join-Path $ProfileDir ".config\powershell"

# Personal modular configuration files, in load order.
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
Write-Host "Personal profile loaded." -ForegroundColor Green

# Load work configuration if present (linked under .config\work).
$WorkEntryPoint = Join-Path $ProfileDir ".config\work\work.ps1"
if (Test-Path $WorkEntryPoint) {
    try {
        . $WorkEntryPoint
        Write-Host "Work profile loaded." -ForegroundColor Green
    } catch {
        Write-Warning "Failed to load work profile: $($_.Exception.Message)"
    }
}
