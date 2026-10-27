# PowerShell Prompt Configuration
# Mirrors bash prompt: [AWS_PROFILE] φ dirname ❯

#region Colors — matching bash vibrant_colors theme
$Colors = @{
    BoldRed    = "`e[38;5;204m"
    BoldGreen  = "`e[38;5;114m"
    BoldBlue   = "`e[38;5;75m"
    BoldCyan   = "`e[38;5;80m"
    BoldWhite  = "`e[38;5;252m"
    BoldOrange = "`e[38;5;209m"
    Reset      = "`e[0m"
}
#endregion

function prompt {
    $lastSuccess = $?
    $exitCode = $LASTEXITCODE
    if ($lastSuccess) { $exitCode = 0 }

    $currentDir = Split-Path -Leaf (Get-Location)
    $userName = $env:USERNAME

    $p = ""

    # Terminal title: user:dirname (OSC 0). Emitted as part of the returned
    # prompt string with a BEL terminator; writing it via Write-Host inside the
    # prompt function throws off PSReadLine's render math and smears long lines.
    $p += "`e]0;${userName}:${currentDir}`a"

    # AWS profile
    if ($env:AWS_PROFILE) {
        $p += "$($Colors.BoldOrange)[$($env:AWS_PROFILE)]$($Colors.Reset) "
    }

    # Status indicator
    if ($exitCode -and $exitCode -ne 0) {
        $p += "$($Colors.BoldRed)φ$($Colors.Reset) "
    } else {
        $p += "$($Colors.BoldGreen)φ$($Colors.Reset) "
    }

    # Current directory
    $p += "$($Colors.BoldCyan)$currentDir$($Colors.Reset)"

    # Trailing prompt char (always white, like bash)
    $p += " $($Colors.BoldWhite)❯$($Colors.Reset) "

    return $p
}
