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
      BinaryCol : byte[]
      TimeStampCol : DateTime
      DecimalCol : decimal
      DoubleCol : double
      SingleCol : single
      Int16Col : int16
      Int32Col : int32
      Int64Col : int64
      Varchar2StringCol : string
      NVarchar2StringCol : string }

  [<Test>]
  let ``default mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<DefaultMapping>
      { DefaultMappingId = 1
        ByteCol = 3uy
        BinaryCol = [| 1uy; 2uy; 3uy; |]
        TimeStampCol = DateTime(2011, 4, 11, 13, 14, 15)
        DecimalCol = 123.456M
        DoubleCol = 123.5
        SingleCol = 123.5f
        Int16Col = 123s
        Int32Col = (int32 Int16.MaxValue) * 2
        Int64Col = (int64 Int32.MaxValue) * 2L
        Varchar2StringCol = "abc"
        NVarchar2StringCol = "あいう" } |> ignore
    let entity = Oracle.find<DefaultMapping> [1]
    printfn "%A" entity
    assert_equal 3uy entity.ByteCol
    assert_equal [| 1uy; 2uy; 3uy; |] entity.BinaryCol
    assert_equal (DateTime(2011, 4, 11, 13, 14, 15)) entity.TimeStampCol
    assert_equal 123.456M entity.DecimalCol
    assert_equal 123.5 entity.DoubleCol
    assert_equal 123.5f entity.SingleCol
    assert_equal 123s entity.Int16Col
    assert_equal ((int32 Int16.MaxValue) * 2) entity.Int32Col
    assert_equal ((int64 Int32.MaxValue) * 2L) entity.Int64Col
    assert_equal "abc" entity.Varchar2StringCol
    assert_equal "あいう" entity.NVarchar2StringCol

  [<Table(Name = "DefaultMapping")>]
  type OptionDefaultMapping =
    { [<Id>]
      DefaultMappingId : int32
      ByteCol : byte option
      BinaryCol : byte[] option
      TimeStampCol : DateTime option
      DecimalCol : decimal option
      DoubleCol : double option
      SingleCol : single option
      Int16Col : int16 option
      Int32Col : int32 option
      Int64Col : int64 option
      Varchar2StringCol : string option
      NVarchar2StringCol : string option }

  [<Test>]
  let ``option default mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<OptionDefaultMapping> 
      { DefaultMappingId = 1
        ByteCol = Some 3uy
        BinaryCol = Some [| 1uy; 2uy; 3uy; |]
        TimeStampCol = Some (DateTime(2011, 4, 11, 13, 14, 15))
        DecimalCol = Some 123.456M
        DoubleCol = Some 123.5
        SingleCol = Some 123.5f
        Int16Col = Some 123s
        Int32Col = Some ((int32 Int16.MaxValue) * 2)
        Int64Col = Some ((int64 Int32.MaxValue) * 2L)
        Varchar2StringCol = Some "abc"
        NVarchar2StringCol = Some "あいう" } |> ignore
    let entity = Oracle.find<OptionDefaultMapping> [1]
    printfn "%A" entity
    assert_equal (Some 3uy) entity.ByteCol
    assert_equal (Some [| 1uy; 2uy; 3uy; |]) entity.BinaryCol
    assert_equal (Some (DateTime(2011, 4, 11, 13, 14, 15))) entity.TimeStampCol
    assert_equal (Some 123.456M) entity.DecimalCol
    assert_equal (Some 123.5) entity.DoubleCol
    assert_equal (Some 123.5f) entity.SingleCol
    assert_equal (Some 123s) entity.Int16Col
    assert_equal (Some ((int32 Int16.MaxValue) * 2)) entity.Int32Col
    assert_equal (Some ((int64 Int32.MaxValue) * 2L)) entity.Int64Col
    assert_equal (Some "abc") entity.Varchar2StringCol
    assert_equal (Some "あいう") entity.NVarchar2StringCol
    Oracle.insert<OptionDefaultMapping> 
      { DefaultMappingId = 2
        ByteCol = None
        BinaryCol = None
        TimeStampCol = None
        DecimalCol = None
        DoubleCol = None
        SingleCol = None
        Int16Col = None
        Int32Col = None
        Int64Col = None
        Varchar2StringCol = None
        NVarchar2StringCol = None } |> ignore
    let entity = Oracle.find<OptionDefaultMapping> [2]
    printfn "%A" entity
    assert_equal None entity.ByteCol
    assert_equal None entity.BinaryCol
    assert_equal None entity.TimeStampCol
    assert_equal None entity.DecimalCol
    assert_equal None entity.DoubleCol
    assert_equal None entity.SingleCol
    assert_equal None entity.Int16Col
    assert_equal None entity.Int32Col
    assert_equal None entity.Int64Col
    assert_equal None entity.Varchar2StringCol
    assert_equal None entity.NVarchar2StringCol

  [<Table(Name = "DefaultMapping")>]
  type NullableDefaultMapping =
    { [<Id>]
      DefaultMappingId : int32
      ByteCol : byte Nullable
      BinaryCol : byte[] 
      TimeStampCol : DateTime Nullable
      DecimalCol : decimal Nullable
      DoubleCol : double Nullable
      SingleCol : single Nullable
      Int16Col : int16 Nullable
      Int32Col : int32 Nullable
      Int64Col : int64 Nullable
      Varchar2StringCol : string
      NVarchar2StringCol : string }

  [<Test>]
  let ``nullable default mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<NullableDefaultMapping> 
      { DefaultMappingId = 1
        ByteCol = Nullable 3uy
        BinaryCol = [| 1uy; 2uy; 3uy; |]
        TimeStampCol = Nullable (DateTime(2011, 4, 11, 13, 14, 15))
        DecimalCol = Nullable 123.456M
        DoubleCol = Nullable 123.5
        SingleCol = Nullable 123.5f
        Int16Col = Nullable 123s
        Int32Col = Nullable ((int32 Int16.MaxValue) * 2)
        Int64Col = Nullable ((int64 Int32.MaxValue) * 2L)
        Varchar2StringCol = "abc"
        NVarchar2StringCol = "あいう" } |> ignore
    let entity = Oracle.find<NullableDefaultMapping> [1]
    printfn "%A" entity
    assert_equal (Nullable 3uy) entity.ByteCol
    assert_equal [| 1uy; 2uy; 3uy; |] entity.BinaryCol
    assert_equal (Nullable (DateTime(2011, 4, 11, 13, 14, 15))) entity.TimeStampCol
    assert_equal (Nullable 123.456M) entity.DecimalCol
    assert_equal (Nullable 123.5) entity.DoubleCol
    assert_equal (Nullable 123.5f) entity.SingleCol
    assert_equal (Nullable 123s) entity.Int16Col
    assert_equal (Nullable ((int32 Int16.MaxValue) * 2)) entity.Int32Col
    assert_equal (Nullable ((int64 Int32.MaxValue) * 2L)) entity.Int64Col
    assert_equal "abc" entity.Varchar2StringCol
    assert_equal "あいう" entity.NVarchar2StringCol
    Oracle.insert<NullableDefaultMapping> 
      { DefaultMappingId = 2
        ByteCol = Nullable ()
        BinaryCol = null
        TimeStampCol = Nullable ()
        DecimalCol = Nullable ()
        DoubleCol = Nullable ()
        SingleCol = Nullable ()
        Int16Col = Nullable ()
        Int32Col = Nullable ()
        Int64Col = Nullable ()
        Varchar2StringCol = null
        NVarchar2StringCol = null } |> ignore
    let entity = Oracle.find<NullableDefaultMapping> [2]
    printfn "%A" entity
    assert_equal (Nullable ()) entity.ByteCol
    assert_equal null entity.BinaryCol
    assert_equal (Nullable ()) entity.TimeStampCol
    assert_equal (Nullable ()) entity.DecimalCol
    assert_equal (Nullable ()) entity.DoubleCol
    assert_equal (Nullable ()) entity.SingleCol
    assert_equal (Nullable ()) entity.Int16Col
    assert_equal (Nullable ()) entity.Int32Col
    assert_equal (Nullable ()) entity.Int64Col
    assert_equal null entity.Varchar2StringCol
    assert_equal null entity.NVarchar2StringCol

  type BooleanMapping =
    { [<Id>]
      BooleanMappingId : int32 
      BooleanCol : bool }

  [<Test>]
  let ``boolean mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<BooleanMapping> { BooleanMappingId = 1; BooleanCol = true } |> ignore
    let entity = Oracle.find<BooleanMapping> [1]
    assert_equal true entity.BooleanCol
    Oracle.insert<BooleanMapping> { BooleanMappingId = 2; BooleanCol = false } |> ignore
    let entity = Oracle.find<BooleanMapping> [2]
    assert_equal false entity.BooleanCol

  [<Table(Name = "BooleanMapping")>]
  type OptionBooleanMapping =
    { [<Id>]
      BooleanMappingId : int32 
      BooleanCol : bool option }

  [<Test>]
  let ``option boolean mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<OptionBooleanMapping> { BooleanMappingId = 1; BooleanCol = Some true } |> ignore
    let entity = Oracle.find<OptionBooleanMapping> [1]
    assert_equal (Some true) entity.BooleanCol
    Oracle.insert<OptionBooleanMapping> { BooleanMappingId = 2; BooleanCol = Some false } |> ignore
    let entity = Oracle.find<OptionBooleanMapping> [2]
    assert_equal (Some false) entity.BooleanCol
    Oracle.insert<OptionBooleanMapping> { BooleanMappingId = 3; BooleanCol = None } |> ignore
    let entity = Oracle.find<OptionBooleanMapping> [3]
    assert_equal None entity.BooleanCol

  [<Table(Name = "BooleanMapping")>]
  type NullableBooleanMapping =
    { [<Id>]
      BooleanMappingId : int32 
      BooleanCol : bool Nullable }

  [<Test>]
  let ``nullable boolean mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<NullableBooleanMapping> { BooleanMappingId = 1; BooleanCol = Nullable true } |> ignore
    let entity = Oracle.find<NullableBooleanMapping> [1]
    assert_equal (Nullable true) entity.BooleanCol
    Oracle.insert<NullableBooleanMapping> { BooleanMappingId = 2; BooleanCol = Nullable false } |> ignore
    let entity = Oracle.find<NullableBooleanMapping> [2]
    assert_equal (Nullable false) entity.BooleanCol
    Oracle.insert<NullableBooleanMapping> { BooleanMappingId = 3; BooleanCol = Nullable() } |> ignore
    let entity = Oracle.find<NullableBooleanMapping> [3]
    assert_equal (Nullable()) entity.BooleanCol

  type TimeSpanMapping =
    { [<Id>]
      TimeSpanMappingId : int32 
      TimeSpanCol : TimeSpan }

  [<Test>]
  let ``TimeSpan mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<TimeSpanMapping> { TimeSpanMappingId = 1; TimeSpanCol = TimeSpan(1, 2, 3, 4, 5) } |> ignore
    let entity = Oracle.find<TimeSpanMapping> [1]
    assert_equal (TimeSpan(1, 2, 3, 4, 5)) entity.TimeSpanCol

  [<Table(Name = "TimeSpanMapping")>]
  type OptionTimeSpanMapping =
    { [<Id>]
      TimeSpanMappingId : int32 
      TimeSpanCol : TimeSpan option }

  [<Test>]
  let ``option TimeSpan mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<OptionTimeSpanMapping> { TimeSpanMappingId = 1; TimeSpanCol = Some (TimeSpan(1, 2, 3, 4, 5)) } |> ignore
    let entity = Oracle.find<OptionTimeSpanMapping> [1]
    assert_equal (Some(TimeSpan(1, 2, 3, 4, 5))) entity.TimeSpanCol
    Oracle.insert<OptionTimeSpanMapping> { TimeSpanMappingId = 2; TimeSpanCol = None } |> ignore
    let entity = Oracle.find<OptionTimeSpanMapping> [2]
    assert_equal None entity.TimeSpanCol

  [<Table(Name = "TimeSpanMapping")>]
  type NullableTimeSpanMapping =
    { [<Id>]
      TimeSpanMappingId : int32 
      TimeSpanCol : TimeSpan Nullable }

  [<Test>]
  let ``nullable TimeSpan mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<NullableTimeSpanMapping> { TimeSpanMappingId = 1; TimeSpanCol = Nullable (TimeSpan(1, 2, 3, 4, 5)) } |> ignore
    let entity = Oracle.find<NullableTimeSpanMapping> [1]
    assert_equal (Nullable (TimeSpan(1, 2, 3, 4, 5))) entity.TimeSpanCol
    Oracle.insert<NullableTimeSpanMapping> { TimeSpanMappingId = 2; TimeSpanCol = Nullable () } |> ignore
    let entity = Oracle.find<NullableTimeSpanMapping> [2]
    assert_equal (Nullable ()) entity.TimeSpanCol

  type ProcTimeSpanSingleParam =
    { [<ProcedureParam(Direction = Direction.InputOutput)>]
      Param1 : TimeSpan }

  [<Test>]
  let ``procedure TimeSpan mapping``() =
    use ts = new TransactionScope()
    let result = Oracle.call<ProcTimeSpanSingleParam> { Param1 = TimeSpan(1, 2, 3, 4, 5) }
    assert_equal (TimeSpan(1, 2, 3, 4, 5)) result.Param1

  type DateMapping =
    { [<Id>]
      DateMappingId : int32 
      DateCol : DateTime }

  [<Test>]
  let ``Date mapping``() =
    use tx = new TransactionScope ()
    Oracle.insert<DateMapping> { DateMappingId = 1; DateCol = DateTime(2011, 1, 2) } |> ignore
    let entity = Oracle.find<DateMapping> [1]
    assert_equal (DateTime(2011, 1, 2)) entity.DateCol

  type ProcDateSingleParam =
    { [<ProcedureParam(Direction = Direction.InputOutput)>]
      Param1 : DateTime }

  [<Test>]
  let ``procedure Date mapping``() =
    use ts = new TransactionScope()
    let result = Oracle.call<ProcDateSingleParam> { Param1 = DateTime(2011, 1, 2) }
    assert_equal (DateTime(2011, 1, 2)) result.Param1