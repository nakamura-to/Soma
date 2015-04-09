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

open System
open System.Data
open System.Data.Common
open System.Transactions
open NUnit.Framework
open Soma.Core

module MappingTest = 

  type DefaultMapping =
    { [<Id>]
      DefaultMappingId : int32 
      ByteCol : byte
      Int16Col : int16
      Int32Col : int32
      Int64Col : int64
      BytesCol : byte[]
      BooleanCol : bool
      DateTimeCol : DateTime
      DecimalCol : decimal
      DoubleCol : double
      SingleCol : single
      StringCol : string 
      GuidCol : Guid }

  [<Test>]
  let ``default mapping``() =
    use tx = new TransactionScope ()
    use con = SQLite.createConnection()
    let guid = Guid.NewGuid()
    SQLite.insert<DefaultMapping>
      con
      { DefaultMappingId = 1 
        ByteCol = 3uy
        Int16Col = 123s
        Int32Col = (int32 Int16.MaxValue) * 2
        Int64Col = (int64 Int32.MaxValue) * 2L
        BytesCol = [| 1uy; 2uy; 3uy; |]
        BooleanCol = true
        DateTimeCol = DateTime(2011, 4, 11, 13, 14, 15)
        DecimalCol = 123.456M
        DoubleCol = 123.5
        SingleCol = 123.5f
        StringCol = "あいう"
        GuidCol = guid } |> ignore
    let entity = SQLite.find<DefaultMapping> con [1]
    printfn "%A" entity
    assert_equal 3uy entity.ByteCol
    assert_equal 123s entity.Int16Col
    assert_equal ((int32 Int16.MaxValue) * 2) entity.Int32Col
    assert_equal ((int64 Int32.MaxValue) * 2L) entity.Int64Col
    assert_equal (DateTime(2011, 4, 11, 13, 14, 15)) entity.DateTimeCol 
    assert_equal 123.456M entity.DecimalCol
    assert_equal 123.5 entity.DoubleCol
    assert_equal 123.5f entity.SingleCol
    assert_equal "あいう" entity.StringCol
    assert_equal guid entity.GuidCol
