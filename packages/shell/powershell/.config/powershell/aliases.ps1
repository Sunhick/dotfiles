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

#region Emacs shortcuts (if using Emacs on Windows)
if (Get-Command emacsclient -ErrorAction SilentlyContinue) {
    function kill-emacs { emacsclient -e "(kill-emacs)" }
    function e { emacsclient -n $args }
    function ec { emacsclient -c $args }
}
#endregion
