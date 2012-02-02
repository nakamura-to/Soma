properties { 
    $baseDir  = resolve-path ..
    $buildDir = "$baseDir\build"
    $workDir = "$buildDir\work"
    $packageDir = "$workDir\package"
    $nugetDir = "$workDir\nuget"
    $nuspecFileName = "$nugetDir\Soma.nuspec"
    $releaseDir = "$projectDir\bin\Release"
    $assemblyFileName = "$baseDir\Soma.Core\AssemblyInfo.fs"
    $assemblyVersionNumber = "0.0.0.0"
}

task default -depends ShowProperties

task ShowProperties {
    "`$baseDir = $baseDir"
    "`$buildDir = $buildDir"
    "`$assemblyFileName = $assemblyFileName"
    "`$workDir = $workDir"
    "`$releaseDir = $releaseDir"
    "`$assemblyVersionNumber = $assemblyVersionNumber"
}

task Clean -depends ShowProperties {
   Set-Location $baseDir
   if (Test-Path -path $workDir)
   {
        Write-Output -ForegroundColor Green "Deleting $workDir"    
        del $workDir -Recurse -Force
   }
   New-Item -Path $workDir -ItemType Directory
   New-Item -Path $packageDir -ItemType Directory
   New-Item -Path $nugetDir -ItemType Directory
   New-Item -Path $nugetDir\lib -ItemType Directory
   New-Item -Path $nugetDir\content -ItemType Directory
   New-Item -Path $nugetDir\content\App_Readme -ItemType Directory
   Copy-Item -Path $buildDir\Soma.nuspec -Destination $nuspecFileName
}

task UpdateVersion -depends Clean {
    $assemblyVersionPattern = 'AssemblyVersion\("[0-9]+(\.([0-9]+|\*)){1,3}"\)'
    $assemblyVersion = 'AssemblyVersion("' + $assemblyVersionNumber + '")';
    (Get-Content $assemblyFileName) -replace $assemblyVersionPattern, $assemblyVersion | Set-Content $assemblyFileName

    $nuspecVersionPattern = '<version>[0-9]+(\.([0-9]+|\*)){1,3}</version>'
    $nuspecVersion = "<version>$assemblyVersionNumber</version>";
    (Get-Content $nuspecFileName) -replace $nuspecVersionPattern, $nuspecVersion | Set-Content $nuspecFileName
}

task Build -depends UpdateVersion {
    Write-Host -ForegroundColor Green "Building"
    Write-Host
    exec { msbuild "/t:Clean;Rebuild" /p:Configuration=Release /p:OutputPath=$workDir\Soma.Core "$baseDir\Soma.Core\Soma.Core.fsproj" } "Error Build"
}

task Test -depends Build {
    Write-Host -ForegroundColor Green "Testing"
    Write-Host
    exec { msbuild "/t:Clean;Rebuild" /p:Configuration=Release /p:OutputPath=$workDir\Soma.Core.UT "$baseDir\Soma.Core.UT\Soma.Core.UT.fsproj" } "Error Build"
    exec { .\build\tools\NUnit-2.5.10.11092\net-2.0\nunit-console.exe "$workDir\Soma.Core.UT\Soma.Core.UT.dll" /framework=$framework /xml:$workDir\Soma.Core.UT\testResult.xml } "Error running $name tests" 
}

task Package -depends Test {
    Write-Host -ForegroundColor Green "Packaging Soma-$assemblyVersionNumber.zip"
    Write-Host
    robocopy $workDir\Soma.Core $packageDir /MIR /NP
    exec { .\build\tools\7za920\7za.exe a -tzip $workDir\Soma-$assemblyVersionNumber.zip $packageDir\* } "Error zipping"
}

task NuGet -depends Package {
    Write-Host -ForegroundColor Green "NuGet"
    Write-Host
    Set-Content -encoding utf8 -path $nugetDir\content\App_Readme\Soma.readme.txt -value @"
* English
Add Reference to the FSharp.Core.dll(Version 4.0.0.0) assembly, if it has not added.

* Japanese
追加されていない場合は、アセンブリFSharp.Core.dll(Version 4.0.0.0)への参照を追加してください。
"@
    Copy-Item -Path $packageDir -Destination $nugetDir\lib\Net40 -recurse
    exec { .\build\tools\NuGet\NuGet.exe pack $nuspecFileName }
    move -Path .\*.nupkg -Destination $workDir
}