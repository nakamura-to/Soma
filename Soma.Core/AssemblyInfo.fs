//----------------------------------------------------------------------------
//
// Copyright (c) 2011 The Soma Team. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.txt file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

module AssemblyInfo
open System.Reflection
open System.Runtime.CompilerServices;
[<assembly:AssemblyDescription("Soma.Core.dll")>]
[<assembly:AssemblyCompany("http://soma.codeplex.com/")>]
[<assembly:AssemblyTitle("Soma.Core.dll")>]
[<assembly:AssemblyCopyright("Copyright the Soma Team.  All rights reserved.")>]
[<assembly:AssemblyProduct("Soma")>]
[<assembly:AssemblyVersion("1.8.0.2")>]
#if DEBUG
[<assembly:InternalsVisibleTo("Soma.Core.UT")>]
#else
[<assembly:InternalsVisibleTo("Soma.Core.UT, PublicKey=002400000480000094000000060200000024000052534131000400000100010055e1b958a7b839882e5af4928916119e3445750726e36ac769e624477175b7660c74a45dcffbc49f40ca4eab78c2a4b906777055ea92868dec5731c145ea4ba51fb4cf3c00c6faa2befb4705296da26850ccec8baa49a282a561e02de8c0fc87ee5196a08f287b38d534c2d6eaf101312cde9fed58dd06884dce7e1e1c4e44a9")>]
#endif
do()
