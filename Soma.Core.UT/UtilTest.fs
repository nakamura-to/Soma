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

namespace Soma.Core.UT

module UtilTest =
  open NUnit.Framework
  open Soma.Core

  [<Test>]
  let ``asOption`` () =
    assert_equal (Some "aaa") (asOption "aaa")
    assert_equal (None) (asOption null)

module DictTest =
  open NUnit.Framework
  open Soma.Core

  [<Test>]
  let ``toList`` () =
    let list = Dict.toList (dict [1, "A"; 2, "B"])
    assert_equal [1, "A"; 2, "B"] list

  [<Test>]
  let ``toSeq`` () =
    let seq = Dict.toSeq (dict [1, "A"; 2, "B"])
    assert_equal [1, "A"; 2, "B"] (Seq.toList seq)

  [<Test>]
  let ``toArray`` () =
    let array = Dict.toArray (dict [1, "A"; 2, "B"])
    assert_equal [|1, "A"; 2, "B"|] array

module MapTest =
  open NUnit.Framework
  open Soma.Core

  [<Test>]
  let ``ofDict`` () =
    let map = Map.ofDict (dict [1, "A"; 2, "B"])
    assert_equal [1, "A"; 2, "B"] (Map.toList map)

module SeqTest =
  open NUnit.Framework
  open Soma.Core

  [<Test>]
  let ``peek`` () =
    let seq = seq {1..3}
    let result = Seq.peek seq
    assert_equal [1, true; 2, true; 3, false] (Seq.toList result)

module ReflectionTest =
  open System
  open NUnit.Framework
  open Soma.Core

  [<Test>]
  let ``isOptionType`` () =
    assert_true (Reflection.isOptionType typeof<int option>)
    assert_true (Reflection.isOptionType typeof<string option>)
    assert_false (Reflection.isOptionType typeof<int>)
    assert_false (Reflection.isOptionType typeof<string>)

  [<Test>]
  let ``makeOption`` () =
    assert_equal (Some 1) (Reflection.makeOption (1, typeof<int option>))
    assert_equal (Some "1") (Reflection.makeOption (1, typeof<string option>))
    assert_equal None (Reflection.makeOption (null, typeof<int option>))
    assert_equal None (Reflection.makeOption (null, typeof<string option>))

  [<Test>]
  let ``getOptionElement`` () =
    assert_equal (box 1, typeof<int>) (Reflection.getOptionElement (Some 1, typeof<int option>))
    assert_equal (box "a", typeof<string>) (Reflection.getOptionElement (Some "a", typeof<string option>))
    assert_equal (null, typeof<int>) (Reflection.getOptionElement (None, typeof<int option>))
    assert_equal (null, typeof<string>) (Reflection.getOptionElement (None, typeof<string option>))

  [<Test>]
  let ``isNullableType`` () =
    assert_true (Reflection.isNullableType typeof<int System.Nullable>)

  [<Test>]
  let ``makeNullable`` () =
    assert_equal (Nullable 1) (Reflection.makeNullable (1, typeof<int Nullable>))
    assert_equal (Nullable<int>()) (Reflection.makeNullable (null, typeof<int Nullable>))

  [<Test>]
  let ``getNullableElement`` () =
    assert_equal (box 1, typeof<int>) (Reflection.getNullableElement (Nullable 1, typeof<int Nullable>))
    assert_equal (null, typeof<int>) (Reflection.getNullableElement (null, typeof<int Nullable>))

  [<Test>]
  let ``isNumberType`` () =
    assert_true (Reflection.isNumberType typeof<int option>)
    assert_false (Reflection.isNumberType typeof<string option>)
    assert_true (Reflection.isNumberType typeof<int>)
    assert_false (Reflection.isNumberType typeof<string>)

  [<Test>]
  let ``zero`` () =
    assert_equal 0 (Reflection.zero typeof<int>)
    assert_equal 0L (Reflection.zero typeof<int64>)
    assert_equal 0M (Reflection.zero typeof<decimal>)
    assert_equal (Some 0) (Reflection.zero typeof<int option>)
    assert_equal (Some 0L) (Reflection.zero typeof<int64 option>)
    assert_equal (Some 0M) (Reflection.zero typeof<decimal option>)
    assert_equal (Nullable 0) (Reflection.zero typeof<int Nullable>)
    assert_equal (Nullable 0L) (Reflection.zero typeof<int64 Nullable>)
    assert_equal (Nullable 0M) (Reflection.zero typeof<decimal Nullable>)

  [<Test>]
  let ``one`` () =
    assert_equal 1 (Reflection.one typeof<int>)
    assert_equal 1L (Reflection.one typeof<int64>)
    assert_equal 1M (Reflection.one typeof<decimal>)
    assert_equal (Some 1) (Reflection.one typeof<int option>)
    assert_equal (Some 1L) (Reflection.one typeof<int64 option>)
    assert_equal (Some 1M) (Reflection.one typeof<decimal option>)
    assert_equal (Nullable 1) (Reflection.one typeof<int Nullable>)
    assert_equal (Nullable 1L) (Reflection.one typeof<int64 Nullable>)
    assert_equal (Nullable 1M) (Reflection.one typeof<decimal Nullable>)

  [<Test>]
  let ``incr`` () =
    assert_equal 2 (Reflection.incr (1, typeof<int>))
    assert_equal 2L (Reflection.incr (1L, typeof<int64>))
    assert_equal 2M (Reflection.incr (1M, typeof<decimal>))
    assert_equal (Some 2) (Reflection.incr (Some 1, typeof<int option>))
    assert_equal (Some 2L) (Reflection.incr (Some 1L, typeof<int64 option>))
    assert_equal (Some 2M) (Reflection.incr (Some 1M, typeof<decimal option>))
    assert_equal (Nullable 2) (Reflection.incr (Nullable 1, typeof<int Nullable>))
    assert_equal (Nullable 2L) (Reflection.incr (Nullable 1L, typeof<int64 Nullable>))
    assert_equal (Nullable 2M) (Reflection.incr (Nullable 1M, typeof<decimal Nullable>))

  [<Test>]
  let ``lessThan`` () =
    assert_true (Reflection.lessThan (1, typeof<int>, 2))
    assert_false (Reflection.lessThan (1, typeof<int>, 1))
    assert_false (Reflection.lessThan (1, typeof<int>, 0))
    assert_true (Reflection.lessThan (1L, typeof<int64>, 2))
    assert_false (Reflection.lessThan (1L, typeof<int64>, 1))
    assert_false (Reflection.lessThan (1L, typeof<int64>, 0))
    assert_true (Reflection.lessThan (Some 1, typeof<int option>, 2))
    assert_false (Reflection.lessThan (Some 1, typeof<int option>, 1))
    assert_false (Reflection.lessThan (Some 1, typeof<int option>, 0))
    assert_true (Reflection.lessThan (None, typeof<int option>, 0))
    assert_true (Reflection.lessThan (Some 1L, typeof<int64 option>, 2))
    assert_false (Reflection.lessThan (Some 1L, typeof<int64 option>, 1))
    assert_false (Reflection.lessThan (Some 1L, typeof<int64 option>, 0))
    assert_true (Reflection.lessThan (None, typeof<int64 option>, 0))
    assert_true (Reflection.lessThan (Nullable 1, typeof<int Nullable>, 2))
    assert_false (Reflection.lessThan (Nullable 1, typeof<int Nullable>, 1))
    assert_false (Reflection.lessThan (Nullable 1, typeof<int Nullable>, 0))
    assert_true (Reflection.lessThan (Nullable(), typeof<int Nullable>, 0))
    assert_true (Reflection.lessThan (Nullable 1L, typeof<int64 Nullable>, 2))
    assert_false (Reflection.lessThan (Nullable 1L, typeof<int64 Nullable>, 1))
    assert_false (Reflection.lessThan (Nullable 1L, typeof<int64 Nullable>, 0))
    assert_true (Reflection.lessThan (Nullable(), typeof<int64 Nullable>, 0))


  [<Test>]
  let ``changeTypeFromSeqToList`` () =
    let seq:seq<obj> = seq { yield box 1; yield box 2 }
    let list = Reflection.changeTypeFromSeqToList typeof<int> seq
    match list with
    | :? (list<int>) ->()
    | _ -> fail ()

  [<Test>]
  let ``changeTypeFromSeqToResizeArray`` () =
    let seq:seq<obj> = seq { yield box 1; yield box 2 }
    let list = Reflection.changeTypeFromSeqToResizeArray typeof<int> seq
    match list with
    | :? (ResizeArray<int>) ->()
    | _ -> fail ()
