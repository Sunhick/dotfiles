# PowerShell History Configuration

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
