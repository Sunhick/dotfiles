# PowerShell FZF Integration

#region FZF Integration (if available)
if (Get-Command fzf -ErrorAction SilentlyContinue) {
    # Ctrl+r history search is handled by PSFzf in work/Modules.ps1

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
