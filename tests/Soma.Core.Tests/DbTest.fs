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

module DbTest = 

  open System
  open System.Data
  open System.ComponentModel
  open System.Collections
  open NUnit.Framework
  open Soma.Core

  let config = 
     { new MsSqlConfig() with
         member this.ConnectionString = "" }

  [<Test>]
  let ``paginate : order by not found`` () =
    try
      Db.paginate<string> config "select * from Employee" [] (2L, 5L)  |> ignore
      fail ()
    with 
    | :? SqlException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA2016" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``find : generic parameter is invalid`` () =
    try
      Db.find<string> config [1] |> ignore
      fail ()
    with 
    | :? DbException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA4002" ex.MessageId
    | ex -> 
      fail ex

  type Hoge = {[<Id>]Id:int32; Name:string; [<Version>]Version:int32}

  [<Test>]
  let ``find : idList is empty`` () =
    try
      Db.find<Hoge> config [] |> ignore
      fail ()
    with 
    | :? DbException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA4004" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``find : idList length is invalid`` () =
    try
      Db.find<Hoge> config [1; 2] |> ignore
      fail ()
    with 
    | :? DbException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA4003" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``MsSqlConfig : object expression`` () =
    let config = 
      { new MsSqlConfig() with
        member this.ConnectionString = "" }
    ()

  [<Test>]
  let ``dynamic : get and set operands`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None)))
    dynamic ? Aaa <- "hoge"
    assert_equal "hoge" dynamic ? aaa
    assert_equal "hoge" dynamic ? AAA

  [<Test>]
  let ``dynamic : get and set operands : auto casting`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None)))
    dynamic ? Aaa <- 1
    assert_true (dynamic ? aaa < 2)
    assert_true (dynamic ? aaa = "1")
    assert_true (dynamic ? aaa = 1)
    assert_true (dynamic ? aaa = Some("1"))
    assert_true (dynamic ? aaa = Some(1))

  [<Test>]
  let ``dynamic : indexer`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None)))
    dynamic.["Aaa"] <- "hoge"
    assert_equal "hoge" dynamic.["aaa"]
    assert_equal "hoge" dynamic.["AAA"]

  [<Test>]
  let ``dynamic : GetCaseSensitiveDict`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None)))
    dynamic ? Aaa <- "aaa"
    dynamic ? Bbb <- "bbb"
    dynamic ? Ccc <- "ccc"
    let dict = dynamic.GetCaseSensitiveDict()
    assert_equal 3 dict.Count
    assert_true (dict.ContainsKey "Aaa")
    assert_false (dict.ContainsKey "AAA")
    assert_true (dict.ContainsKey "Bbb")
    assert_false (dict.ContainsKey "BBB")
    assert_true (dict.ContainsKey "Ccc")
    assert_false (dict.ContainsKey "CCC")
    assert_equal "aaa" dict.["Aaa"]
    assert_equal "bbb" dict.["Bbb"]
    assert_equal "ccc" dict.["Ccc"]

  [<Test>]
  let ``dynamic : ContainsKey`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None)))
    dynamic ? Aaa <- "aaa"
    dynamic ? Bbb <- "bbb"
    dynamic ? Ccc <- "ccc"
    assert_true (dynamic.ContainsKey "AAA")
    assert_true (dynamic.ContainsKey "aaa")

  [<Test>]
  let ``dynamic : Remove`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None)))
    dynamic ? Aaa <- "aaa"
    dynamic ? Bbb <- "bbb"
    dynamic ? Ccc <- "ccc"
    assert_equal 3 dynamic.Count
    dynamic.Remove "AAA" |> ignore
    assert_equal 2 dynamic.Count

  [<Test>]
  let ``dynamic : as IDictionary`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None))) :> IDictionary
    dynamic.["AAA"] <- "aaa"
    assert_equal "aaa" dynamic.["AAA"]
    assert_equal "aaa" dynamic.["aaa"]

  [<Test>]
  let ``dynamic : convert exception`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None)))
    dynamic.["AAA"] <- "aaa"
    try
      let num:int = dynamic?aaa
      ()
    with
    | :? DbException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA4027" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``dynamic : ICustomTypeDescriptor`` () =
    let dynamic = dynamic(MsSqlDialect(Func<Type, DbType option>(fun t -> None))) :> IDictionary
    dynamic.["AAA"] <- "aaa"
    dynamic.["BBB"] <- 10
    let typeDescriptor = dynamic :?> ICustomTypeDescriptor
    let props = typeDescriptor.GetProperties()
    let prop = props.Find("AAA", false)
    assert_equal "aaa" (prop.GetValue(dynamic))
    prop.SetValue(dynamic, "xxx")
    assert_equal "xxx" (prop.GetValue(dynamic))
    let prop = props.Find("BBB", false)
    assert_equal 10 (prop.GetValue(dynamic))
    prop.SetValue(dynamic, 20)
    assert_equal 20 (prop.GetValue(dynamic))

  [<Test>]
  let ``Conversion : toIdList`` () =
    assert_equal [box "hoge"] (Conversion.toIdList "hoge")
    assert_equal [box "hoge"; box "foo"] (Conversion.toIdList ["hoge"; "foo"])
    assert_equal [box "hoge"; box 1] (Conversion.toIdList [box "hoge"; box 1])
    assert_equal [box 1] (Conversion.toIdList 1)

  [<Test>]
  let ``Conversion : toExprCtxt : dict`` () =
    let hash = Hashtable()
    hash.["aaa"] <- "hoge"
    hash.[100] <- 200
    hash.["xxx"] <- null
    let exprCtxt = Conversion.toExprCtxt hash
    assert_equal (box "hoge", typeof<obj>) (exprCtxt.["aaa"])
    assert_equal (box 200, typeof<obj>) (exprCtxt.["100"])
    assert_equal (box null, typeof<obj>) (exprCtxt.["xxx"])

  [<Test>]
  let ``Conversion : toExprCtxt : list`` () =
    let exprCtxt = Conversion.toExprCtxt ["aaa" @= "hoge"; "100" @= 200; "xxx" @= null]
    assert_equal (box "hoge", typeof<string>) (exprCtxt.["aaa"])
    assert_equal (box 200, typeof<int>) (exprCtxt.["100"])
    assert_equal (box null, typeof<obj>) (exprCtxt.["xxx"])

  [<Test>]
  let ``Conversion : toExprCtxt : exception`` () =
    try
      Conversion.toExprCtxt "" |> ignore
    with
    | :? DbException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA4020" ex.MessageId
    | ex -> 
      fail ex