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

namespace Soma.Core.IT

[<AutoOpen>]
module TestTool =

  open NUnit.Framework

  let (|Unit|_|) x = if x = box (()) then Some () else None
  
  let fail = function
    | Unit -> Assert.Fail()
    | x -> Assert.Fail(sprintf "%A" x)

  let assert_equal (expected:obj) (actual:obj) =
    if expected <> actual then 
      let message = sprintf "Expected: %A\nBut was : %A" expected actual
      Assert.Fail(message)

  let assert_true condition  = 
    if not condition then 
      let message = sprintf "Expected: true\nBut was : %A" condition
      Assert.Fail(message)

