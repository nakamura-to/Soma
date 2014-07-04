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

module ExpressionTest =
  open System
  open System.Collections
  open System.Collections.Generic
  open Microsoft.FSharp.Text.Lexing
  open Microsoft.FSharp.Quotations
  open Microsoft.FSharp.Reflection
  open NUnit.Framework
  open Soma.Core
  open Soma.Core.Text
  open Soma.Core.ExpressionAst

  type Hoge = { Name : string }

  type レコード = { 名前 : string }

  type レコード2 = { ``名前[]`` : string }

  [<Test>]
  let ``parse Unit`` () =
    match Expression.parse "()" with
    | Factor(Unit) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Null`` () =
    match Expression.parse "null" with
    | Factor(Null) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Boolean : true`` () =
    match Expression.parse "true" with
    | Factor(Boolean true) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Boolean : false`` () =
    match Expression.parse "false" with
    | Factor(Boolean false) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Byte`` () =
    match Expression.parse "1uy" with
    | Factor(Byte 1uy) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Byte : fail`` () =
    try
      Expression.parse "1000uy" |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1000" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``parse SByte`` () =
    match Expression.parse "1y" with
    | Factor(SByte 1y) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Int16`` () =
    match Expression.parse "1s" with
    | Factor(Int16 1s) -> ()
    | x -> fail x

  [<Test>]
  let ``parse UInt16`` () =
    match Expression.parse "1us" with
    | Factor(UInt16 1us) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Int32`` () =
    match Expression.parse "1" with
    | Factor(Int32 1) -> ()
    | x -> fail x

  [<Test>]
  let ``parse UInt32`` () =
    match Expression.parse "1u" with
    | Factor(UInt32 1u) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Int64`` () =
    match Expression.parse "1L" with
    | Factor(Int64 1L) -> ()
    | x -> fail x

  [<Test>]
  let ``parse UInt64`` () =
    match Expression.parse "1UL" with
    | Factor(UInt64 1UL) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Single`` () =
    match Expression.parse "1.0f" with
    | Factor(Single 1.0f) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Double`` () =
    match Expression.parse "1.0" with
    | Factor(Double 1.0) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Decimal`` () =
    match Expression.parse "1.234M" with
    | Factor(Decimal 1.234M) -> ()
    | x -> fail x

  [<Test>]
  let ``parse String`` () =
    match Expression.parse "'abc'" with
    | Factor(String "abc") -> ()
    | x -> fail x

  [<Test>]
  let ``parse String : single quotation`` () =
    match Expression.parse "'a''bc'" with
    | Factor(String "a'bc") -> ()
    | x -> fail x

  [<Test>]
  let ``parse String : double quotation`` () =
    match Expression.parse "'a\"bc'" with
    | Factor(String "a\"bc") -> ()
    | x -> fail x

  [<Test>]
  let ``parse String : back slash`` () =
    match Expression.parse "'a\bc'" with
    | Factor(String "a\bc") -> ()
    | x -> fail x

  [<Test>]
  let ``parse String : escaped back slash`` () =
    match Expression.parse "'a\\bc'" with
    | Factor(String "a\\bc") -> ()
    | x -> fail x

  [<Test>]
  let ``parse String : new line`` () =
    match Expression.parse "'a\nbc'" with
    | Factor(String "a\nbc") -> ()
    | x -> fail x

  [<Test>]
  let ``parse String : escaped new line`` () =
    match Expression.parse "'a\\nbc'" with
    | Factor(String "a\\nbc") -> ()
    | x -> fail x

  [<Test>]
  let ``parse Var`` () =
    match Expression.parse "abc" with
    | Factor(Var ("abc", _)) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Var : delimited ident`` () =
    match Expression.parse "[あいうえお]" with
    | Factor(Var ("あいうえお", _)) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Add`` () =
    match Expression.parse "1 + 1" with
    | Add(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Sub`` () =
    match Expression.parse "1 - 1" with
    | Sub(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Mul`` () =
    match Expression.parse "1 * 1" with
    | Mul(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Div`` () =
    match Expression.parse "1 / 1" with
    | Div(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Mod`` () =
    match Expression.parse "1 % 1" with
    | Mod(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Equal`` () =
    match Expression.parse "1 = 1" with
    | Equal(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Equal 2`` () =
    match Expression.parse "1 == 1" with
    | Equal(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse NotEqual`` () =
    match Expression.parse "1 <> 1" with
    | NotEqual(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse NotEqual 2`` () =
    match Expression.parse "1 != 1" with
    | NotEqual(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse LessThan`` () =
    match Expression.parse "1 < 1" with
    | LessThan(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse GreaterThan`` () =
    match Expression.parse "1 > 1" with
    | GreaterThan(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse LessThanOrEqual`` () =
    match Expression.parse "1 <= 1" with
    | LessThanOrEqual(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse GreaterThanOrEqual`` () =
    match Expression.parse "1 >= 1" with
    | GreaterThanOrEqual(Factor(Int32 1), Factor(Int32 1), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse AndAlso`` () =
    match Expression.parse "true && true" with
    | AndAlso(Factor(Boolean true), Factor(Boolean true), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse OrElse`` () =
    match Expression.parse "true || true" with
    | OrElse(Factor(Boolean true), Factor(Boolean true), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Not`` () =
    match Expression.parse "not true" with
    | Not("not", Factor(Boolean true), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Not 2`` () =
    match Expression.parse "! true" with
    | Not("!", Factor(Boolean true), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Property`` () =
    match Expression.parse "aaa.Length" with
    | Factor(Property(Var ("aaa", _), "Length", _)) -> ()
    | x -> fail x

  [<Test>]
  let ``parse StaticProperty`` () =
    match Expression.parse "$System.DateTime$.Now" with
    | Factor(StaticProperty("System.DateTime", "Now", _)) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Application : 1 argument`` () =
    match Expression.parse "id 1" with
    | Application(Factor(Var ("id", _)), Int32 1, _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Application : 2 arguments`` () =
    match Expression.parse "add 1 2" with
    | Application(Application(Factor(Var ("add", _)), Int32 1, _), Int32 2, _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Tuple : 2 elements`` () =
    match Expression.parse "1, 2" with
    | Tuple([Int32(1); Int32(2)], _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Tuple : 3 elements`` () =
    match Expression.parse "1, 2, 3" with
    | Tuple([Int32(1); Int32(2); Int32(3)], _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Tuple : empty element`` () =
    try 
      Expression.parse "10, ,1" |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1019" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``parse : In`` () =
    match Expression.parse "aaa in bbb" with
    | In(Var("aaa", _), Factor(Var("bbb", _)), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Parens`` () =
    match Expression.parse "(1)" with
    | Factor(Parens(Factor(Int32 1))) -> ()
    | x -> fail x

  [<Test>]
  let ``parse Parens : extra a right paren`` () =
    try 
      Expression.parse "(1)) && 2 = 2" |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1019" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``parse : precedence for "true && false && not true"`` () =
    match Expression.parse "true && false && not true" with
    | AndAlso(AndAlso(Factor(Boolean true), Factor(Boolean false), _), Not("not", Factor(Boolean true), _), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse : precedence for "true && false || not true"`` () =
    match Expression.parse "true && false || not true" with
    | OrElse(AndAlso(Factor(Boolean true), Factor(Boolean false), _), Not("not", Factor(Boolean true), _), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse : precedence for "true || false && not true"`` () =
    match Expression.parse "true || false && not true" with
    | OrElse(Factor(Boolean true), AndAlso(Factor(Boolean false), Not("not", Factor(Boolean true), _), _), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse : precedence for "true || false || not true"`` () =
    match Expression.parse "true || false || not true" with
    | OrElse(OrElse(Factor(Boolean true), Factor(Boolean false), _), Not("not", Factor(Boolean true), _), _) -> ()
    | x -> fail x

  [<Test>]
  let ``parse : unsupported token`` () =
    try 
      Expression.parse "1 ? 1" |> ignore
      fail ()
    with
    | :? ExpressionException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA1018" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``parse : location`` () =
    match Expression.parse "1 = 1" with
    | Equal(Factor(Int32 1), Factor(Int32 1), loc) -> 
      assert_equal { pos_fname = "Expression"; pos_lnum = 1; pos_bol = 0; pos_cnum = 2; } loc
    | x ->
      fail x

  let parser = Func<string, ExpressionAst.Expression>(Expression.parse)

  [<Test>]
  let ``evaluate Unit`` () =
    let result = Expression.evaluate "()" Map.empty parser
    assert_equal () (fst result)
    assert_equal typeof<obj> (snd result) 

  [<Test>]
  let ``evaluate Null`` () =
    let result = Expression.evaluate "null" Map.empty parser
    assert_equal null (fst result)
    assert_equal typeof<obj> (snd result) 

  [<Test>]
  let ``evaluate Int32`` () =
    let result = Expression.evaluate "1" Map.empty parser
    assert_equal 1 (fst result)
    assert_equal typeof<Int32> (snd result) 

  [<Test>]
  let ``evaluate String`` () =
    let result = Expression.evaluate "'1'" Map.empty parser
    assert_equal "1" (fst result)
    assert_equal typeof<String> (snd result) 
        
  [<Test>]
  let ``evaluate Boolean`` () =
    let result = Expression.evaluate "true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<Boolean> (snd result) 

  [<Test>]
  let ``evaluate Int32 Add Int32`` () =
    let result = Expression.evaluate "1 + 1" Map.empty parser
    assert_equal 2 (fst result)
    assert_equal typeof<Int32> (snd result) 

  [<Test>]
  let ``evaluate String Add String`` () =
    let result = Expression.evaluate "'abc' + 'd'" Map.empty parser
    assert_equal "abcd" (fst result)
    assert_equal typeof<String> (snd result) 

  [<Test>]
  let ``evaluate Float Add Float`` () =
    let result = Expression.evaluate "1.0 + 1.0" Map.empty parser
    assert_equal 2.0 (fst result)
    assert_equal typeof<float> (snd result) 

  [<Test>]
  let ``evaluate Int64 Add Int64`` () =
    let result = Expression.evaluate "1L + 1L" Map.empty parser
    assert_equal 2L (fst result)
    assert_equal typeof<int64> (snd result) 

  [<Test>]
  let ``evaluate UInt64 Add UInt64`` () =
    let result = Expression.evaluate "1UL + 1UL" Map.empty parser
    assert_equal 2UL (fst result)
    assert_equal typeof<uint64> (snd result) 

  [<Test>]
  let ``evaluate UInt32 Add UInt32`` () =
    let result = Expression.evaluate "1u + 1u" Map.empty parser
    assert_equal 2u (fst result)
    assert_equal typeof<uint32> (snd result) 

  [<Test>]
  let ``evaluate Int16 Add Int16`` () =
    let result = Expression.evaluate "1s + 1s" Map.empty parser
    assert_equal 2s (fst result)
    assert_equal typeof<int16> (snd result) 

  [<Test>]
  let ``evaluate UInt16 Add UInt16`` () =
    let result = Expression.evaluate "1us + 1us" Map.empty parser
    assert_equal 2us (fst result)
    assert_equal typeof<uint16> (snd result) 

  [<Test>]
  let ``evaluate SByte Add SByte`` () =
    let result = Expression.evaluate "1y + 1y" Map.empty parser
    assert_equal 2y (fst result)
    assert_equal typeof<sbyte> (snd result) 

  [<Test>]
  let ``evaluate Byte Add Byte`` () =
    let result = Expression.evaluate "1uy + 1uy" Map.empty parser
    assert_equal 2uy (fst result)
    assert_equal typeof<byte> (snd result) 

  [<Test>]
  let ``evaluate Decimal Add Decimal`` () =
    let result = Expression.evaluate "1.1M + 1.1M" Map.empty parser
    assert_equal 2.2M (fst result)
    assert_equal typeof<decimal> (snd result) 

  [<Test>]
  let ``evaluate Int32 Equal Int32`` () =
    let result = Expression.evaluate "1 = 1" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "1 = 0" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate String Equal String`` () =
    let result = Expression.evaluate "'aaa' = 'aaa'" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'aaa' = 'bbb'" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Boolean Equal Boolean`` () =
    let result = Expression.evaluate "true = true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false = false" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "true = false" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false = true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Int32 NotEqual Int32`` () =
    let result = Expression.evaluate "1 <> 1" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "1 <> 0" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate String NotEqual String`` () =
    let result = Expression.evaluate "'aaa' <> 'aaa'" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'aaa' <> 'bbb'" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Boolean NotEqual Boolean`` () =
    let result = Expression.evaluate "true <> true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false <> false" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "true <> false" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false <> true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Int32 LessThan Int32`` () =
    let result = Expression.evaluate "1 < 1" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "1 < 0" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "0 < 1" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate String LessThan String`` () =
    let result = Expression.evaluate "'aaa' < 'aaa'" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'aaa' < 'bbb'" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'bbb' < 'aaa'" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Boolean LessThan Boolean`` () =
    let result = Expression.evaluate "true < true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "true < false" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false < true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 


  [<Test>]
  let ``evaluate LessThan : different types`` () =
    try 
      Expression.evaluate "10 < true" Map.empty parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1001" ex.MessageId
    | ex -> 
      fail ex 

  [<Test>]
  let ``evaluate LessThan : not IComparable`` () =
    try 
      Expression.evaluate "a < a" (dict ["a", (obj(), typeof<obj>)] ) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA1007" ex.MessageId
    | ex -> fail ex
  
  [<Test>]
  let ``evaluate Int32 GreaterThan Int32`` () =
    let result = Expression.evaluate "1 > 1" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "1 > 0" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "0 > 1" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate String GreaterThan String`` () =
    let result = Expression.evaluate "'aaa' > 'aaa'" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'aaa' > 'bbb'" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'bbb' > 'aaa'" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Boolean GreaterThan Boolean`` () =
    let result = Expression.evaluate "true > true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "true > false" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false > true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Int32 LessThanOrEqual Int32`` () =
    let result = Expression.evaluate "1 <= 1" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "1 <= 0" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "0 <= 1" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate String LessThanOrEqual String`` () =
    let result = Expression.evaluate "'aaa' <= 'aaa'" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'aaa' <= 'bbb'" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'bbb' <= 'aaa'" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Boolean LessThanOrEqual Boolean`` () =
    let result = Expression.evaluate "true <= true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "true <= false" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false <= true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Int32 GreaterThanOrEqual Int32`` () =
    let result = Expression.evaluate "1 >= 1" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "1 >= 0" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "0 >= 1" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate String GreaterThanOrEqual String`` () =
    let result = Expression.evaluate "'aaa' >= 'aaa'" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'aaa' >= 'bbb'" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "'bbb' >= 'aaa'" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Boolean GreaterThanOrEqual Boolean`` () =
    let result = Expression.evaluate "true >= true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "true >= false" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false >= true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate AndAlso`` () =
    let result = Expression.evaluate "true && true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "true && false" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false && true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false && false" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate AndAlso : different type`` () =
    try
      Expression.evaluate "true && 1" Map.empty parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1009" ex.MessageId 
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate AndAlso : not boolean`` () =
    try
      Expression.evaluate "1 && 1" Map.empty parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1009" ex.MessageId 
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate AndAlso : lazy evaluation : lhs not evaluated`` () =
    let result = Expression.evaluate "value <> null && value.Length > 0" (dict ["value", (null, typeof<obj>)]) parser 
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate AndAlso : lazy evaluation : lhs evaluated`` () =
    let result = Expression.evaluate "value <> null && value.Length > 0" (dict ["value", (box "abc", typeof<string>)]) parser 
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate OrElse`` () =
    let result = Expression.evaluate "true || true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "true || false" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false || true" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "false || false" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate OrElse : lazy evaluation : lhs not evaluated`` () =
    let result = Expression.evaluate "value == null || value.Length == 0" (dict ["value", (null, typeof<obj>)]) parser 
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate OrElse : lazy evaluation : lhs evaluated`` () =
    let result = Expression.evaluate "value == null || value.Length == 0" (dict ["value", (box "abc", typeof<string>)]) parser 
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Not`` () =
    let result = Expression.evaluate "not true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "not false" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Not 2`` () =
    let result = Expression.evaluate "! true" Map.empty parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "! false" Map.empty parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Not : not boolean 1`` () =
    try
      Expression.evaluate "not 1" Map.empty parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1011" ex.MessageId 
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Not : not boolean 2`` () =
    try
      Expression.evaluate "! 1" Map.empty parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1011" ex.MessageId 
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate StaticProperty`` () =
    let result = Expression.evaluate "$System.Type$.Delimiter" Map.empty parser
    assert_equal Type.Delimiter (fst result)
    assert_equal typeof<char> (snd result) 

  [<Test>]
  let ``evaluate StaticProperty : field`` () =
    let result = Expression.evaluate "$System.Int32$.MaxValue" Map.empty parser
    assert_equal Int32.MaxValue (fst result)
    assert_equal typeof<int> (snd result) 
    let result = Expression.evaluate "$int$.MaxValue" Map.empty parser
    assert_equal Int32.MaxValue (fst result)
    assert_equal typeof<int> (snd result) 
    let result = Expression.evaluate "$System.DayOfWeek$.Friday" Map.empty parser
    assert_equal DayOfWeek.Friday (fst result)
    assert_equal typeof<DayOfWeek> (snd result) 

  [<Test>]
  let ``evaluate StaticProperty : not either static property or static field`` () =
    try 
      Expression.evaluate "$System.String$.xxx" Map.empty parser |> ignore
      fail ()
    with 
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1004" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Property`` () =
    let result = Expression.evaluate "'abc'.Length" Map.empty parser
    assert_equal 3 (fst result)
    assert_equal typeof<int> (snd result) 

  [<Test>]
  let ``evaluate Property : field`` () =
    let result = Expression.evaluate "a.Name" (map ["a", (box { Hoge.Name = "aaa" }, typeof<Hoge>);] ) parser
    assert_equal "aaa" (fst result)
    assert_equal typeof<string> (snd result) 

  [<Test>]
  let ``evaluate Property : not either property or field`` () =
    try 
      Expression.evaluate "'abc'.xxx" Map.empty parser |> ignore
      fail ()
    with 
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1003" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Property : GenericDictionary`` () =
    let dict = Dictionary<string, obj>()
    dict.["Aaa"] <- "Hoge"
    let result = Expression.evaluate "a.Aaa" (map ["a", (box dict, dict.GetType());] ) parser
    assert_equal "Hoge" (fst result)
    assert_equal typeof<obj> (snd result) 

  type Hoge2 = { dict : IDictionary<string, string> }

  [<Test>]
  let ``evaluate Property : GenericDictionary : nested`` () =
    let hoge = { dict = Dictionary<string, string>() }
    hoge.dict.["Aaa"] <- "Hoge"
    let result = Expression.evaluate "a.dict.Aaa" (map ["a", (box hoge, hoge.GetType());] ) parser
    assert_equal "Hoge" (fst result)
    assert_equal typeof<obj> (snd result) 

  [<Test>]
  let ``evaluate Property : Dictionary`` () =
    let dict = Hashtable()
    dict.["Aaa"] <- "Hoge"
    let result = Expression.evaluate "a.Aaa" (map ["a", (box dict, dict.GetType());] ) parser
    assert_equal "Hoge" (fst result)
    assert_equal typeof<obj> (snd result) 

  [<Test>]
  let ``evaluate Property : GenericDictionary : null`` () =
    try
      Expression.evaluate "a.Aaa" (map ["a", (null, typeof<Hashtable>);] ) parser |> ignore 
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1026" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Property : GenericDictionary : key not found`` () =
    let dict = Dictionary<string, obj>()
    try
      Expression.evaluate "a.Aaa" (map ["a", (box dict, dict.GetType());] ) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1003" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Property : Dictionary : key not found`` () =
    let dict = Hashtable()
    let result = Expression.evaluate "a.Aaa" (map ["a", (box dict, dict.GetType());] ) parser
    assert_equal null (fst result)
    assert_equal typeof<obj> (snd result) 

  [<Test>]
  let ``evaluate Property : Map`` () =
    let m = Map.ofList [("Aaa", "Hoge")]
    let result = Expression.evaluate "a.Aaa" (map ["a", (box m, m.GetType());] ) parser
    assert_equal "Hoge" (fst result)
    assert_equal typeof<obj> (snd result) 

  [<Test>]
  let ``evaluate Property : Map : key not found`` () =
    let m = Map.empty
    try
      Expression.evaluate "a.Aaa" (map ["a", (box m, m.GetType());] ) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1003" ex.MessageId
    | ex -> 
      fail ex
  [<Test>]
  let ``evaluate Property : CaseInsensitiveDynamicObject`` () =
    let dynamic = CaseInsensitiveDynamicObject(MsSqlDialect())
    dynamic?Aaa <- "Hoge"
    let result = Expression.evaluate "a.Aaa" (map ["a", (box dynamic, dynamic.GetType());] ) parser
    assert_equal "Hoge" (fst result)
    assert_equal typeof<obj> (snd result) 

  [<Test>]
  let ``evaluate Property : CaseInsensitiveDynamicObject : key not found`` () =
    let dynamic = CaseInsensitiveDynamicObject(MsSqlDialect())
    try
      Expression.evaluate "a.Aaa" (map ["a", (box dynamic, dynamic.GetType());] ) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1003" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Var`` () =
    let result = Expression.evaluate "a" (dict ["a", (box "abc", typeof<string>)]) parser
    assert_equal "abc" (fst result)
    assert_equal typeof<string> (snd result) 

  [<Test>]
  let ``evaluate Var : can't resolved`` () =
    try 
      Expression.evaluate "a" Map.empty parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1006" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Ident.Property : string`` () =
    let result = Expression.evaluate "a.Length" (dict ["a", (box "abc", typeof<string>)]) parser
    assert_equal 3 (fst result)
    assert_equal typeof<int> (snd result) 

  [<Test>]
  let ``evaluate Ident.Property : string : null`` () =
    try
      Expression.evaluate "a.Length" (dict ["a", (null, typeof<string>)]) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1026" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Ident.Property : record`` () =
    let result = Expression.evaluate "a.Name" (dict ["a", (box { Hoge.Name = "hoge" }, typeof<Hoge>)]) parser
    assert_equal "hoge" (fst result)
    assert_equal typeof<string> (snd result)

  [<Test>]
  let ``evaluate Ident.Property : record : null`` () =
    try
      Expression.evaluate "a.Name" (dict ["a", (null, typeof<Hoge>)]) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1026" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Ident.Property : square branket`` () =
    let result = Expression.evaluate "[レコード].[名前]" (dict ["レコード", (box { レコード.名前 = "hoge" }, typeof<レコード>)]) parser
    assert_equal "hoge" (fst result)
    assert_equal typeof<string> (snd result)

  [<Test>]
  let ``evaluate Ident.Property : escaped square branket`` () =
    let result = Expression.evaluate "[レコード2].[名前[]]]" (dict ["レコード2", (box { レコード2.``名前[]`` = "hoge" }, typeof<レコード2>)]) parser
    assert_equal "hoge" (fst result)
    assert_equal typeof<string> (snd result)

  [<Test>]
  let ``evaluate Ident.Property : unclosed square brancket`` () =
    try
      Expression.evaluate "[レコード].[名前" (dict ["レコード", (box { レコード.名前 = "hoge" }, typeof<レコード>)]) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1027" ex.MessageId
    | ex -> 
      fail ex


  [<Test>]
  let ``evaluate Ident.Property : option`` () =
    let result = Expression.evaluate "a.IsSome" (dict ["a", (box (Some "foo"), typeof<string option>)]) parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "a.IsNone" (dict ["a", (box (Some "foo"), typeof<string option>)]) parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "a.IsNone" (dict ["a", (box (unbox<string option> None), typeof<string option>)]) parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "a.Value" (dict ["a", (box (Some "foo"), typeof<string option>)]) parser
    assert_equal "foo" (fst result)
    assert_equal typeof<string> (snd result) 

  [<Test>]
  let ``evaluate Ident.Property : option : null`` () =
    try
      Expression.evaluate "a.Value" (dict ["a", (box (unbox<string option> None), typeof<string option>)]) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1026" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Ident.Property : nullable`` () =
    let result = Expression.evaluate "a.HasValue" (dict ["a", (box (Nullable 1), typeof<int Nullable>)]) parser
    assert_equal true (fst result)
    assert_equal typeof<bool> (snd result) 
    let result = Expression.evaluate "a.HasValue" (dict ["a", (box (Nullable ()), typeof<int Nullable>)]) parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Ident.Property : nullable : null`` () =
    let result = Expression.evaluate "a.HasValue" (dict ["a", (box (Nullable ()), typeof<int Nullable>)]) parser
    assert_equal false (fst result)
    assert_equal typeof<bool> (snd result) 

  [<Test>]
  let ``evaluate Application : 1 arg`` () =
    let incr x = x + 1
    let context = dict ["incr", (box incr, incr.GetType());]
    let result = Expression.evaluate "incr 1" context parser
    assert_equal 2 (fst result)
    assert_equal typeof<int> (snd result) 

  [<Test>]
  let ``evaluate Application : 2 args`` () =
    let add x y = x + y
    let context = dict ["add", (box add, add.GetType());]
    let result = Expression.evaluate "add 1 2" context parser
    assert_equal 3 (fst result)
    assert_equal typeof<int> (snd result) 

  [<Test>]
  let ``evaluate Application : 3 args`` () =
    let add x y z = x + y + z
    let context = dict ["add", (box add, add.GetType());]
    let result = Expression.evaluate "add 1 2 3" context parser
    assert_equal 6 (fst result)
    assert_equal typeof<int> (snd result) 

  [<Test>]
  let ``evaluate Tuple`` () =
    let tuple = 1, "abc", true
    let context = dict ["tuple", (box tuple, tuple.GetType());]
    let result = Expression.evaluate "tuple" context parser
    assert_equal (1, "abc", true) (fst result)
    assert_equal typeof<Tuple<int, string, bool>> (snd result)

  [<Test>]
  let ``evaluate Parens`` () =
    let add x y = x + y
    let context = dict ["add", (box add, add.GetType())]
    let result = Expression.evaluate "add (add 1 2) 3" context parser
    assert_equal 6 (fst result)
    assert_equal typeof<int> (snd result)

  [<Test>]
  let ``evaluate In`` () =
    let list = [1; 2; 3]
    let context = dict ["list", (box list, list.GetType());]
    let result = Expression.evaluate "x in list" context parser
    match fst result with
    | :? Tuple<string, seq<obj>> as x ->
      assert_equal "x" x.Item1
      assert_equal 0 (Seq.compareWith (compare) (Seq.cast<int> x.Item2) (list))
    | _ -> fail()
    assert_equal typeof<Tuple<string, seq<obj>>> (snd result)

  [<Test>]
  let ``evaluate In : illegal left hand operand`` () =
    let list = [1; 2; 3]
    try 
      Expression.evaluate "'aaa' in bbb" (dict ["bbb", (box list, list.GetType())]) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1021" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate In : illegal right hand operand`` () =
    try 
      Expression.evaluate "aaa in bbb" (dict ["bbb", (box 100, typeof<int>)]) parser |> ignore
      fail ()
    with
    | :? ExpressionException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA1022" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``evaluate Option`` () =
    let opt = Some 1
    let result = Expression.evaluate "opt" (map ["opt", (box opt, opt.GetType())]) parser
    assert_equal (Some 1) (fst result)
    assert_equal typeof<int option> (snd result) 

  type Foo =
    | Aaa = 1

  [<Test>]
  let ``evaluate Enum`` () =
    let context = dict ["a", (box Foo.Aaa, typeof<Foo>)]
    let result = Expression.evaluate "a" context parser
    assert_equal Foo.Aaa (fst result)
    assert_equal typeof<Foo> (snd result)
