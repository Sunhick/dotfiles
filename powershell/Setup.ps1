# PowerShell Profile Setup Script
# Installs and configures the modular PowerShell profile with bash-like settings.
#
# Output mirrors the repo's Makefile / make/log.mk style:
#   OK   <msg>   (green)   WARN <msg> (yellow)   FAIL <msg> (red)
#   Section headers in cyan, plus a pass/warn/fail summary at the end.

param(
    [switch]$Force,
    [switch]$InstallModules,
    [switch]$InstallTools
)

# ── Logging helpers (match make/log.mk) ────────────────────────────
$script:Pass = 0
$script:Warn = 0
$script:Fail = 0

function Log-Ok     { param([string]$Msg) $script:Pass++; Write-Host ("  OK   {0}" -f $Msg) -ForegroundColor Green }
function Log-Warn   { param([string]$Msg) $script:Warn++; Write-Host ("  WARN {0}" -f $Msg) -ForegroundColor Yellow }
function Log-Fail   { param([string]$Msg) $script:Fail++; Write-Host ("  FAIL {0}" -f $Msg) -ForegroundColor Red }
function Log-Info   { param([string]$Msg) Write-Host $Msg -ForegroundColor Cyan }
function Log-Header { param([string]$Msg) Write-Host ""; Write-Host $Msg -ForegroundColor Cyan }

function Show-Summary {
    Log-Header "Summary"
    Write-Host ("  {0} passed, {1} warnings, {2} errors" -f $script:Pass, $script:Warn, $script:Fail)
    if ($script:Fail -gt 0) {
        Write-Host ""
        Write-Host "  Some steps failed. Review the errors above." -ForegroundColor Red
        exit 1
    } elseif ($script:Warn -gt 0) {
        Write-Host ""
        Write-Host "  All done, but some optional steps were skipped." -ForegroundColor Yellow
    } else {
        Write-Host ""
        Write-Host "  Everything looks great!" -ForegroundColor Green
    }
}

Log-Info "PowerShell Profile Setup"
Log-Info "========================"

# ── Preconditions ──────────────────────────────────────────────────
Log-Header "Requirements"
if ($PSVersionTable.PSVersion.Major -lt 5) {
    Log-Fail "PowerShell 5.0 or higher is required (found $($PSVersionTable.PSVersion))"
    Show-Summary
}
Log-Ok "PowerShell $($PSVersionTable.PSVersion)"

# Get profile path
$ProfilePath = $PROFILE.CurrentUserCurrentHost
$ProfileDir  = Split-Path $ProfilePath

# ── Profile directory ──────────────────────────────────────────────
Log-Header "Profile"
Log-Info "  Target: $ProfilePath"

if (!(Test-Path $ProfileDir)) {
    New-Item -ItemType Directory -Path $ProfileDir -Force | Out-Null
    Log-Ok "MKDIR profile directory"
}

# Backup existing profile if it exists
if (Test-Path $ProfilePath) {
    if (!$Force) {
        $response = Read-Host "  Profile already exists. Backup and replace? (y/N)"
        if ($response -ne 'y' -and $response -ne 'Y') {
            Log-Warn "Setup cancelled by user"
            Show-Summary
        }
    }

    $BackupPath = "$ProfilePath.backup.$(Get-Date -Format 'yyyyMMdd-HHmmss')"
    Copy-Item $ProfilePath $BackupPath
    Log-Ok "BACKUP $BackupPath"
}

# ── Link main profile ──────────────────────────────────────────────
Log-Header "Linking profile"

# Create .config directory structure in profile directory
$ConfigDir = Join-Path $ProfileDir ".config\powershell"
if (!(Test-Path $ConfigDir)) {
    New-Item -ItemType Directory -Path $ConfigDir -Force | Out-Null
    Log-Ok "MKDIR $ConfigDir"
}

# Copy or link the main profile
$SourcePath = Resolve-Path "Microsoft.PowerShell_profile.ps1"
try {
    New-Item -ItemType SymbolicLink -Path $ProfilePath -Target $SourcePath -Force | Out-Null
    Log-Ok "LINK Microsoft.PowerShell_profile.ps1"
} catch {
    Copy-Item "Microsoft.PowerShell_profile.ps1" $ProfilePath -Force
    Log-Warn "COPY Microsoft.PowerShell_profile.ps1 (symlink failed; may need admin rights)"
}

# Copy or link modular configuration files
$ConfigFiles = @(
    "environment.ps1",
    "prompt.ps1",
    "aliases.ps1",
    "history.ps1",
    "fzf.ps1",
    "work.ps1"
)

foreach ($ConfigFile in $ConfigFiles) {
    $SourceConfigPath = Join-Path ".config\powershell" $ConfigFile
    $TargetConfigPath = Join-Path $ConfigDir $ConfigFile

    if (Test-Path $SourceConfigPath) {
        try {
            $ResolvedSource = Resolve-Path $SourceConfigPath
            New-Item -ItemType SymbolicLink -Path $TargetConfigPath -Target $ResolvedSource -Force | Out-Null
            Log-Ok "LINK $ConfigFile"
        } catch {
            Copy-Item $SourceConfigPath $TargetConfigPath -Force
            Log-Warn "COPY $ConfigFile (symlink failed)"
        }
    } else {
        Log-Warn "SKIP $ConfigFile (not found)"
    }
}

# ── PowerShell modules (PSGallery) ─────────────────────────────────
if ($InstallModules) {
    Log-Header "PowerShell modules"
    $ModulesToInstall = @('PSReadLine', 'posh-git', 'Terminal-Icons')
    foreach ($Module in $ModulesToInstall) {
        try {
            Install-Module -Name $Module -Scope CurrentUser -Force -AllowClobber
            Log-Ok "MODULE $Module"
        } catch {
            Log-Fail "MODULE $Module ($($_.Exception.Message))"
        }
    }
}

# ── CLI tools (winget, declarative manifest) ───────────────────────
if ($InstallTools) {
    Log-Header "CLI tools (winget)"
    if (Get-Command winget -ErrorAction SilentlyContinue) {
        $Manifest = Join-Path $PSScriptRoot "winget-packages.json"
        if (Test-Path $Manifest) {
            # winget import is idempotent: already-installed packages are skipped
            winget import --import-file $Manifest --accept-package-agreements --accept-source-agreements --ignore-versions
            if ($LASTEXITCODE -eq 0) {
                Log-Ok "IMPORT winget-packages.json"
            } else {
                Log-Fail "IMPORT winget-packages.json (exit code $LASTEXITCODE)"
            }
        } else {
            Log-Fail "IMPORT winget-packages.json (manifest not found)"
        }
    } else {
        Log-Warn "winget not found (install 'App Installer' from the Microsoft Store)"
    }
}

# ── Post-install guidance ──────────────────────────────────────────
Log-Header "Configuration structure"
Write-Host "  Main profile: $ProfilePath"
Write-Host "  Config files: $ConfigDir"
Write-Host "    - environment.ps1  Environment variables and PATH"
Write-Host "    - prompt.ps1       Custom prompt with git integration"
Write-Host "    - aliases.ps1      Command aliases and functions"
Write-Host "    - history.ps1      History and PSReadLine settings"
Write-Host "    - fzf.ps1          Fuzzy finder integration"

Log-Header "Apply changes"
Write-Host "  Restart PowerShell, or run:  . `$PROFILE"

Log-Header "Next steps"
if (!$InstallModules) {
    Write-Host "  PowerShell modules:  .\Setup.ps1 -InstallModules"
}
if (!$InstallTools) {
    Write-Host "  CLI tools:           .\Setup.ps1 -InstallTools"
}
Write-Host "  Install a Nerd Font for terminal icons; Windows Terminal recommended."

Show-Summary
