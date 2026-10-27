# PowerShell Aliases and Functions

#region Navigation shortcuts
function .. { Set-Location .. }
function ... { Set-Location "../.." }
function .... { Set-Location "../../.." }
function ..... { Set-Location "../../../.." }
function ~ { Set-Location $env:USERPROFILE }

# `cd -` style previous-directory toggle (PowerShell has no built-in)
$global:__LastLocation = $null
function Set-LocationPrevious {
    $current = (Get-Location).Path
    if ($global:__LastLocation) {
        Set-Location $global:__LastLocation
    }
    $global:__LastLocation = $current
}
Set-Alias -Name cd- -Value Set-LocationPrevious
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
# Note: these are exposed under distinct names rather than shadowing the
# builtins (find/grep/cat) whose flags differ and which scripts rely on.
if (Get-Command bat -ErrorAction SilentlyContinue) {
    function batcat { bat $args }
}
if (Get-Command fd -ErrorAction SilentlyContinue) {
    function fdfind { fd $args }
}
if (Get-Command rg -ErrorAction SilentlyContinue) {
    function rgrep { rg $args }
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
#endregion

# Note: `code`/`kiro` no-sandbox aliases live in the work profile
# (work/.config/work/powershell/Aliases.ps1) since they depend on
# work-only functions.
