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
      BinaryCol : byte[]
      ImageBinaryCol : byte[]
      VarbinaryBinaryCol : byte[]
      [<Version(VersionKind.Computed)>]
      RowversionBinaryCol : byte[]
      BooleanCol : bool
      DateTimeCol : DateTime
      DecimalCol : decimal
      NumericDecimalCol : decimal
      MoneyDecimalCol : decimal
      DoubleCol : double
      SingleCol : single
      NVarcharStringCol : string
      NTextStringCol : string 
      GuidCol : Guid }

  [<Test>]
  let ``default mapping``() =
    use tx = new TransactionScope ()
    use con = MsSqlCe.createConnection()
    let guid = Guid.NewGuid()
    MsSqlCe.insert<DefaultMapping>
      con
      { DefaultMappingId = 1 
        ByteCol = 3uy
        Int16Col = 123s
        Int32Col = (int32 Int16.MaxValue) * 2
        Int64Col = (int64 Int32.MaxValue) * 2L
        BinaryCol = [| 1uy; 2uy; 3uy; |]
        ImageBinaryCol = [| 1uy; 2uy; 3uy; |]
        VarbinaryBinaryCol = [| 1uy; 2uy; 3uy; |]
        RowversionBinaryCol = [| |]
        BooleanCol = true
        DateTimeCol = DateTime(2011, 4, 11, 13, 14, 15)
        DecimalCol = 123.456M
        NumericDecimalCol = 123.456M
        MoneyDecimalCol = 123.456M
        DoubleCol = 123.5
        SingleCol = 123.5f
        NVarcharStringCol = "あいう"
        NTextStringCol = "あいう" 
        GuidCol = guid } |> ignore
    let entity = MsSqlCe.find<DefaultMapping> con [1]
    printfn "%A" entity
    assert_equal 3uy entity.ByteCol
    assert_equal 123s entity.Int16Col
    assert_equal ((int32 Int16.MaxValue) * 2) entity.Int32Col
    assert_equal ((int64 Int32.MaxValue) * 2L) entity.Int64Col
    assert_equal [|1uy; 2uy; 3uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|] entity.BinaryCol
    assert_equal [|1uy; 2uy; 3uy |] entity.ImageBinaryCol
    assert_equal [|1uy; 2uy; 3uy |] entity.VarbinaryBinaryCol
    assert_true (not <| Array.isEmpty entity.RowversionBinaryCol)
    assert_equal (DateTime(2011, 4, 11, 13, 14, 15)) entity.DateTimeCol 
    assert_equal 123.456M entity.DecimalCol
    assert_equal 123.456M entity.NumericDecimalCol
    assert_equal 123.456M entity.MoneyDecimalCol
    assert_equal 123.5 entity.DoubleCol
    assert_equal 123.5f entity.SingleCol
    assert_equal "あいう" entity.NVarcharStringCol 
    assert_equal "あいう" entity.NTextStringCol
    assert_equal guid entity.GuidCol
