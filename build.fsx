#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.Testing.NUnit3
open System

// Properties
let UNKOWN_VERSION = "UNKOWN"
let version = getBuildParamOrDefault "version" UNKOWN_VERSION

Target "Clean" (fun _ ->
    CleanDirs ["./temp/"]
)

Target "Build" (fun _ ->
    if version <> UNKOWN_VERSION then
        UpdateAttributes  "./Soma.Core/AssemblyInfo.fs"
            [Attribute.Version version]
    !! "Soma.Core/*.fsproj"
      |> MSBuildRelease "" "Rebuild"
      |> Log "Build-Output: "
)

Target "BuildTest" (fun _ ->
    !! "Soma.Core.UT/*.fsproj"
      |> MSBuildDebug "" "Rebuild"
      |> Log "TestBuild-Output: "
)

Target "RunTest" (fun _ ->
    let baseDir = "Soma.Core.UT/bin/Debug"
    !! (baseDir + "/Soma.Core.UT.dll") 
    |> NUnit3 (fun p ->
        { p with
            ResultSpecs  = [baseDir + "/TestResult.xml"]
            TimeOut = TimeSpan.FromMinutes 20. })
)

Target "Nuget" (fun _ ->
    Paket.Pack(id)
)

// Dependencies
"Clean"
  ==> "Build"
  ==> "BuildTest"
  ==> "RunTest"
  ==> "Nuget"

// start build
RunTargetOrDefault "Nuget"
