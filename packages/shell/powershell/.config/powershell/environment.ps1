# PowerShell Environment Configuration

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
