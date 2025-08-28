# PowerShell Prompt Configuration

#region Colors
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
#endregion

#region Custom Prompt Function
function prompt {
    # Capture and preserve the last exit code immediately
    # Capture the success/failure of the last command
    $lastSuccess = $?
    $exitCode = $LASTEXITCODE

    # Reset LASTEXITCODE for successful commands
    if ($lastSuccess) {
        $global:LASTEXITCODE = 0
        $exitCode = 0
    }

    $promptString = ""

    # Show AWS profile if set
    if ($env:AWS_PROFILE) {
        $promptString += "$($Colors.BoldOrange)[$($env:AWS_PROFILE)]$($Colors.Reset) "
    }

    # Status indicator based on last exit code
    if ($exitCode -and $exitCode -ne 0) {
        $promptString += "$($Colors.BoldRed)»$($Colors.Reset) "
    } else {
        $promptString += "$($Colors.BoldGreen)»$($Colors.Reset) "
    }

    # Current directory (just the name, not full path)
    $currentDir = Split-Path -Leaf (Get-Location)
    $promptString += "$($Colors.BoldCyan)$currentDir$($Colors.Reset)"

    # Git branch (if in a git repository) - suppress error output and preserve exit code
    if (Get-Command git -ErrorAction SilentlyContinue) {
        try {
            $branch = git rev-parse --abbrev-ref HEAD 2>$null
            if ($LASTEXITCODE -eq 0 -and $branch) {
                $promptString += " $($Colors.BoldBlue)($branch)$($Colors.Reset)"
            }
        } catch {
            # Not in a git repository or git command failed
        }
    }

    # Final prompt character with color based on command success
    if (-not $lastSuccess -or ($exitCode -and $exitCode -ne 0)) {
        $promptString += " $($Colors.BoldRed)»$($Colors.Reset) "
    } else {
        $promptString += " $($Colors.BoldGreen)»$($Colors.Reset) "
    }


    return $promptString
}
#endregion
