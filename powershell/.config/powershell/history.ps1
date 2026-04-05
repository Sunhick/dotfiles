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

# PSReadLine is auto-loaded in PS 7.4+, no need to import
# History settings
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadLineOption -MaximumHistoryCount $MaximumHistoryCount
Set-PSReadLineOption -HistorySavePath $HistoryPath

# History search
Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward
#endregion
