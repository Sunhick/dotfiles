# PowerShell Profile - Replicated from Bash Configuration
# Place this file in your PowerShell profile directory
# Run: $PROFILE to see the expected location

#region XDG Base Directory Specification (Windows equivalent)
$env:XDG_CONFIG_HOME = if ($env:XDG_CONFIG_HOME) { $env:XDG_CONFIG_HOME } else { "$env:USERPROFILE\.config" }
$env:XDG_DATA_HOME = if ($env:XDG_DATA_HOME) { $env:XDG_DATA_HOME } else { "$env:USERPROFILE\.local\share" }
$env:XDG_CACHE_HOME = if ($env:XDG_CACHE_HOME) { $env:XDG_CACHE_HOME } else { "$env:USERPROFILE\.cache" }
$env:XDG_STATE_HOME = if ($env:XDG_STATE_HOME) { $env:XDG_STATE_HOME } else { "$env:USERPROFILE\.local\state" }

# Create directories if they don't exist
@($env:XDG_CONFIG_HOME, $env:XDG_DATA_HOME, $env:XDG_CACHE_HOME, $env:XDG_STATE_HOME) | ForEach-Object {
    if (!(Test-Path $_)) { New-Item -ItemType Directory -Path $_ -Force | Out-Null }
}
#endregion

#region Environment Variables
$env:EDITOR = "code"  # Change to your preferred editor
$env:VISUAL = $env:EDITOR
$env:LESS = "-R -F -X"
$env:GREP_OPTIONS = "--color=auto"
$env:GREP_COLOR = "1;32"

# GPG TTY equivalent (if using GPG on Windows)
if (Get-Command gpg -ErrorAction SilentlyContinue) {
    $env:GPG_TTY = "CON"
}
#endregion

#region Colors and Prompt
# Define colors (PowerShell equivalent of bash colors)
$Colors = @{
    Red = "`e[38;5;196m"
    Green = "`e[38;5;40m"
    BoldBlack = "`e[38;5;238m"
    BoldRed = "`e[38;5;203m"
    BoldGreen = "`e[38;5;77m"
    BoldYellow = "`e[38;5;227m"
    BoldBlue = "`e[38;5;75m"
    BoldPurple = "`e[38;5;135m"
    BoldCyan = "`e[1;1;38;5;40m"
    BoldWhite = "`e[38;5;255m"
    BoldOrange = "`e[38;5;208m"
    Reset = "`e[0m"
}

# Custom prompt function
function prompt {
    $lastExitCode = $LASTEXITCODE
    $promptString = ""

    # Show AWS profile if set
    if ($env:AWS_PROFILE) {
        $promptString += "$($Colors.BoldOrange)[$($env:AWS_PROFILE)]$($Colors.Reset) "
    }

    # Status indicator based on last exit code
    if ($lastExitCode -and $lastExitCode -ne 0) {
        $promptString += "$($Colors.BoldRed)»$($Colors.Reset) "
    } else {
        $promptString += "$($Colors.BoldGreen)»$($Colors.Reset) "
    }

    # Current directory (just the name, not full path)
    $currentDir = Split-Path -Leaf (Get-Location)
    $promptString += "$($Colors.BoldCyan)$currentDir$($Colors.Reset)"

    # Git branch (if in a git repository)
    if (Get-Command git -ErrorAction SilentlyContinue) {
        try {
            $branch = git rev-parse --abbrev-ref HEAD 2>$null
            if ($branch) {
                $promptString += " $($Colors.BoldBlue)($branch)$($Colors.Reset)"
            }
        } catch {
            # Not in a git repository
        }
    }

    # Final prompt character
    $promptString += " $($Colors.BoldWhite)»$($Colors.Reset) "

    return $promptString
}
#endregion

#region Aliases - Navigation
Set-Alias -Name ".." -Value "Set-Location .."
function ... { Set-Location "../.." }
function .... { Set-Location "../../.." }
function ..... { Set-Location "../../../.." }
function ~ { Set-Location $env:USERPROFILE }
function - { Set-Location - }

# List directory contents
function l { Get-ChildItem -Force }
function la { Get-ChildItem -Force }
function ll { Get-ChildItem }
function lsa { Get-ChildItem -Force }

# Git shortcuts
function g { git $args }
function gs { git status $args }
function ga { git add $args }
function gc { git commit $args }
function gp { git push $args }
function gl { git pull $args }
function gd { git diff $args }
function gb { git branch $args }
function gco { git checkout $args }

# Utility shortcuts
function h { Get-History }
function j { Get-Job }
function c { Clear-Host }
function reload { . $PROFILE }

# Modern tool replacements (if available)
if (Get-Command bat -ErrorAction SilentlyContinue) {
    function cat { bat $args }
}
if (Get-Command fd -ErrorAction SilentlyContinue) {
    function find { fd $args }
}
if (Get-Command rg -ErrorAction SilentlyContinue) {
    function grep { rg $args }
}

# Emacs shortcuts (if using Emacs on Windows)
if (Get-Command emacsclient -ErrorAction SilentlyContinue) {
    function kill-emacs { emacsclient -e "(kill-emacs)" }
    function e { emacsclient -n $args }
    function ec { emacsclient -c $args }
}
#endregion

#region History Configuration
# PowerShell history settings
$MaximumHistoryCount = 10000

# Set custom history file location (XDG compliant)
$HistoryPath = Join-Path $env:XDG_STATE_HOME "powershell\history.txt"
$HistoryDir = Split-Path $HistoryPath
if (!(Test-Path $HistoryDir)) {
    New-Item -ItemType Directory -Path $HistoryDir -Force | Out-Null
}

# Import PSReadLine for better history and editing
if (Get-Module -ListAvailable PSReadLine) {
    Import-Module PSReadLine

    # History settings
    Set-PSReadLineOption -HistorySearchCursorMovesToEnd
    Set-PSReadLineOption -MaximumHistoryCount $MaximumHistoryCount
    Set-PSReadLineOption -HistorySavePath $HistoryPath

    # Emacs-style key bindings (similar to bash)
    Set-PSReadLineOption -EditMode Emacs

    # History search
    Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
    Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward

    # Tab completion
    Set-PSReadLineKeyHandler -Key Tab -Function Complete
    Set-PSReadLineOption -CompletionQueryItems 100
}
#endregion

#region Path Management
# Add common directories to PATH if they exist
$PathsToAdd = @(
    "$env:USERPROFILE\.local\bin"
    "$env:USERPROFILE\bin"
    "$env:LOCALAPPDATA\Programs"
)

foreach ($PathToAdd in $PathsToAdd) {
    if ((Test-Path $PathToAdd) -and ($env:PATH -notlike "*$PathToAdd*")) {
        $env:PATH = "$PathToAdd;$env:PATH"
    }
}
#endregion

#region FZF Integration (if available)
if (Get-Command fzf -ErrorAction SilentlyContinue) {
    # Basic FZF integration
    function fzf-history {
        $selection = Get-History | ForEach-Object { $_.CommandLine } | fzf
        if ($selection) {
            [Microsoft.PowerShell.PSConsoleReadLine]::InvokePrompt()
            [Microsoft.PowerShell.PSConsoleReadLine]::Insert($selection)
        }
    }

    # Bind Ctrl+R to history search (like bash)
    if (Get-Module PSReadLine) {
        Set-PSReadLineKeyHandler -Key Ctrl+r -ScriptBlock { fzf-history }
    }
}
#endregion

#region Welcome Message
Write-Host "PowerShell profile loaded successfully!" -ForegroundColor Green
Write-Host "Bash-style aliases and functions are now available." -ForegroundColor Cyan
#endregion
