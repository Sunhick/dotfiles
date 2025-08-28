# PowerShell FZF Integration

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

    # Additional FZF functions
    function fzf-cd {
        $selection = Get-ChildItem -Directory -Recurse -Depth 3 | Select-Object -ExpandProperty FullName | fzf
        if ($selection) {
            Set-Location $selection
        }
    }

    function fzf-edit {
        $selection = Get-ChildItem -File -Recurse -Depth 3 | Select-Object -ExpandProperty FullName | fzf
        if ($selection) {
            & $env:EDITOR $selection
        }
    }
}
#endregion
