param($assemblyVersionNumber)

cls

Import-Module '.\tools\psake\psake.psm1'
Invoke-psake '.\build.ps1' -framework 4.0 -properties @{"assemblyVersionNumber" = $assemblyVersionNumber} NuGet
Remove-Module psake