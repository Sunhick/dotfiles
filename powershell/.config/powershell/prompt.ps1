# PowerShell Prompt Configuration
# Mirrors bash prompt: [AWS_PROFILE] ❯ dirname ❯

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

    # Terminal title: user:dirname
    $ESC = [char]27
    Write-Host "$ESC]0;${userName}:${currentDir}$ESC\" -NoNewline

    $p = ""

    # AWS profile
    if ($env:AWS_PROFILE) {
        $p += "$($Colors.BoldOrange)[$($env:AWS_PROFILE)]$($Colors.Reset) "
    }

    # Status indicator
    if ($exitCode -and $exitCode -ne 0) {
        $p += "$($Colors.BoldRed)❯$($Colors.Reset) "
    } else {
        $p += "$($Colors.BoldGreen)❯$($Colors.Reset) "
    }

    # Current directory
    $p += "$($Colors.BoldCyan)$currentDir$($Colors.Reset)"

    # Trailing prompt char (always white, like bash)
    $p += " $($Colors.BoldWhite)❯$($Colors.Reset) "

    return $p
}
