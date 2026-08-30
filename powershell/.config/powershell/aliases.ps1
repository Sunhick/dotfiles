# PowerShell Aliases and Functions

#region Navigation shortcuts
Set-Alias -Name ".." -Value "Set-Location .."
function ... { Set-Location "../.." }
function .... { Set-Location "../../.." }
function ..... { Set-Location "../../../.." }
function ~ { Set-Location $env:USERPROFILE }
function - { Set-Location - }
#endregion

#region List directory contents
function l { Get-ChildItem -Force }
function la { Get-ChildItem -Force }
function ll { Get-ChildItem }
function lsa { Get-ChildItem -Force }
#endregion

#region Git shortcuts
function g { git $args }
function gs { git status $args }
function ga { git add $args }
function gc { git commit $args }
function gp { git push $args }
function gl { git pull $args }
function gd { git diff $args }
function gb { git branch $args }
function gco { git checkout $args }
#endregion

#region Utility shortcuts
function h { Get-History }
function j { Get-Job }
function c { Clear-Host }
function reload { . $PROFILE }
#endregion

#region Modern tool replacements (if available)
if (Get-Command bat -ErrorAction SilentlyContinue) {
    function cat { bat $args }
}
if (Get-Command fd -ErrorAction SilentlyContinue) {
    function find { fd $args }
}
if (Get-Command rg -ErrorAction SilentlyContinue) {
    function grep { rg $args }
}
#endregion

#region Brazil shortcuts (Amazon build system)
if (Get-Command brazil -ErrorAction SilentlyContinue) {
    # Core
    function b { brazil $args }
    function bb { brazil-build $args }
    function bbb { brazil-build build $args }
    function bre { brazil-runtime-exec $args }

    # Workspace
    function bws { brazil ws $args }
    function bwu { brazil ws use $args }
    function bwc { brazil ws create $args }
    function bwsy { brazil ws sync $args }
    function bwr { brazil ws remove $args }
    function bwsh { brazil ws show $args }

    # Version sets
    function bvs { brazil vs $args }

    # Recursive command
    function brc { brazil-recursive-cmd $args }

    # Common build targets
    function bbc { brazil-build clean $args }
    function bbr { brazil-build release $args }
    function bbt { brazil-build test $args }

    # Setup tooling
    function bst { brazil-setup-tool $args }
}
#endregion

#region Emacs shortcuts (if using Emacs on Windows)
if (Get-Command emacsclient -ErrorAction SilentlyContinue) {
    function kill-emacs { emacsclient -e "(kill-emacs)" }
    function e { emacsclient -n $args }
    function ec { emacsclient -c $args }
}


Set-Alias -Name code -Value code-no-sandbox
#endregion
