namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Soma.Core")>]
[<assembly: AssemblyProductAttribute("Soma")>]
[<assembly: AssemblyDescriptionAttribute(".NET ORM, primarily for F#")>]
[<assembly: AssemblyVersionAttribute("1.8.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.8.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.8.0.0"
