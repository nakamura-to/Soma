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
      DateDateTimeCol : DateTime
      DateTime2Col : DateTime
      SmallDateTimeCol : DateTime
      DateTimeOffsetCol : DateTimeOffset
      DecimalCol : decimal
      NumericDecimalCol : decimal
      MoneyDecimalCol : decimal
      SmallMoneyDecimalCol : decimal
      DoubleCol : double
      SingleCol : single
      VarcharStringCol : string
      NVarcharStringCol : string
      NTextStringCol : string 
      GuidCol : Guid }

  [<Test>]
  let ``default mapping``() =
    use tx = new TransactionScope ()
    let guid = Guid.NewGuid()
    MsSql.insert<DefaultMapping>
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
        DateDateTimeCol = DateTime(2011, 4, 11, 13, 14, 15)
        DateTime2Col = DateTime(2011, 4, 11, 13, 14, 15)
        SmallDateTimeCol = DateTime(2011, 4, 11, 13, 14, 15)
        DateTimeOffsetCol = DateTimeOffset(DateTime(2011, 4, 11, 13, 14, 15))
        DecimalCol = 123.456M
        NumericDecimalCol = 123.456M
        MoneyDecimalCol = 123.456M
        SmallMoneyDecimalCol = 123.456M
        DoubleCol = 123.5
        SingleCol = 123.5f
        VarcharStringCol = "abc"
        NVarcharStringCol = "あいう"
        NTextStringCol = "あいう" 
        GuidCol = guid } |> ignore
    let entity = MsSql.find<DefaultMapping> [1]
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
    assert_equal (DateTime(2011, 4, 11, 0, 0, 0)) entity.DateDateTimeCol
    assert_equal (DateTime(2011, 4, 11, 13, 14, 15)) entity.DateTime2Col 
    assert_equal (DateTime(2011, 4, 11, 13, 14, 0)) entity.SmallDateTimeCol 
    assert_equal (DateTimeOffset(DateTime(2011, 4, 11, 13, 14, 15))) entity.DateTimeOffsetCol
    assert_equal 123.456M entity.DecimalCol
    assert_equal 123.456M entity.NumericDecimalCol
    assert_equal 123.456M entity.MoneyDecimalCol
    assert_equal 123.456M entity.SmallMoneyDecimalCol 
    assert_equal 123.5 entity.DoubleCol
    assert_equal 123.5f entity.SingleCol
    assert_equal "abc" entity.VarcharStringCol 
    assert_equal "あいう" entity.NVarcharStringCol 
    assert_equal "あいう" entity.NTextStringCol
    assert_equal guid entity.GuidCol

  [<Table(Name = "DefaultMapping")>]
  type OptionDefaultMapping =
    { [<Id>]
      DefaultMappingId : int32 
      ByteCol : byte option
      Int16Col : int16 option
      Int32Col : int32 option
      Int64Col : int64 option
      BinaryCol : byte[] option
      ImageBinaryCol : byte[] option
      VarbinaryBinaryCol : byte[] option
      [<Version(VersionKind.Computed)>]
      RowversionBinaryCol : byte[] option
      BooleanCol : bool option
      DateTimeCol : DateTime option
      DateDateTimeCol : DateTime option
      DateTime2Col : DateTime option
      SmallDateTimeCol : DateTime option
      DateTimeOffsetCol : DateTimeOffset option
      DecimalCol : decimal option
      NumericDecimalCol : decimal option
      MoneyDecimalCol : decimal option
      SmallMoneyDecimalCol : decimal option
      DoubleCol : double option
      SingleCol : single option
      VarcharStringCol : string option
      NVarcharStringCol : string option
      NTextStringCol : string option
      GuidCol : Guid option }

  [<Test>]
  let ``option mapping : some``() =
    use tx = new TransactionScope ()
    let guid = Guid.NewGuid()
    MsSql.insert<OptionDefaultMapping>
      { DefaultMappingId = 1 
        ByteCol = Some 3uy
        Int16Col = Some 123s
        Int32Col = Some ((int32 Int16.MaxValue) * 2)
        Int64Col = Some ((int64 Int32.MaxValue) * 2L)
        BinaryCol = Some [| 1uy; 2uy; 3uy; |]
        ImageBinaryCol = Some [| 1uy; 2uy; 3uy; |]
        VarbinaryBinaryCol = Some [| 1uy; 2uy; 3uy; |]
        RowversionBinaryCol = Some [| |]
        BooleanCol = Some true
        DateTimeCol = Some (DateTime(2011, 4, 11, 13, 14, 15))
        DateDateTimeCol = Some (DateTime(2011, 4, 11, 13, 14, 15))
        DateTime2Col = Some (DateTime(2011, 4, 11, 13, 14, 15))
        SmallDateTimeCol = Some (DateTime(2011, 4, 11, 13, 14, 15))
        DateTimeOffsetCol = Some (DateTimeOffset(DateTime(2011, 4, 11, 13, 14, 15)))
        DecimalCol = Some 123.456M
        NumericDecimalCol = Some 123.456M
        MoneyDecimalCol = Some 123.456M
        SmallMoneyDecimalCol = Some 123.456M
        DoubleCol = Some 123.5
        SingleCol = Some 123.5f
        VarcharStringCol = Some "abc"
        NVarcharStringCol = Some "あいう"
        NTextStringCol = Some "あいう" 
        GuidCol = Some guid } |> ignore
    let entity = MsSql.find<OptionDefaultMapping> [1]
    printfn "%A" entity
    assert_equal (Some 3uy) entity.ByteCol
    assert_equal (Some 123s) entity.Int16Col
    assert_equal (Some ((int32 Int16.MaxValue) * 2)) entity.Int32Col
    assert_equal (Some ((int64 Int32.MaxValue) * 2L)) entity.Int64Col
    assert_equal (Some [|1uy; 2uy; 3uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]) entity.BinaryCol
    assert_equal (Some [|1uy; 2uy; 3uy |]) entity.ImageBinaryCol
    assert_equal (Some [|1uy; 2uy; 3uy |]) entity.VarbinaryBinaryCol
    assert_true (not <| Array.isEmpty entity.RowversionBinaryCol.Value)
    assert_equal (Some (DateTime(2011, 4, 11, 13, 14, 15))) entity.DateTimeCol 
    assert_equal (Some (DateTime(2011, 4, 11, 0, 0, 0))) entity.DateDateTimeCol
    assert_equal (Some (DateTime(2011, 4, 11, 13, 14, 15))) entity.DateTime2Col 
    assert_equal (Some (DateTime(2011, 4, 11, 13, 14, 0))) entity.SmallDateTimeCol 
    assert_equal (Some (DateTimeOffset(DateTime(2011, 4, 11, 13, 14, 15)))) entity.DateTimeOffsetCol
    assert_equal (Some 123.456M) entity.DecimalCol
    assert_equal (Some 123.456M) entity.NumericDecimalCol
    assert_equal (Some 123.456M) entity.MoneyDecimalCol
    assert_equal (Some 123.456M) entity.SmallMoneyDecimalCol 
    assert_equal (Some 123.5) entity.DoubleCol
    assert_equal (Some 123.5f) entity.SingleCol
    assert_equal (Some "abc") entity.VarcharStringCol 
    assert_equal (Some "あいう") entity.NVarcharStringCol 
    assert_equal (Some "あいう") entity.NTextStringCol
    assert_equal (Some guid) entity.GuidCol

  [<Test>]
  let ``option mapping : none``() =
    use tx = new TransactionScope ()
    let guid = Guid.NewGuid()
    MsSql.insert<OptionDefaultMapping>
      { DefaultMappingId = 1 
        ByteCol = None
        Int16Col = None
        Int32Col = None
        Int64Col = None
        BinaryCol = None
        ImageBinaryCol = None
        VarbinaryBinaryCol = None
        RowversionBinaryCol = None
        BooleanCol = None
        DateTimeCol = None
        DateDateTimeCol = None
        DateTime2Col = None
        SmallDateTimeCol = None
        DateTimeOffsetCol = None
        DecimalCol = None
        NumericDecimalCol = None
        MoneyDecimalCol = None
        SmallMoneyDecimalCol = None
        DoubleCol = None
        SingleCol = None
        VarcharStringCol = None
        NVarcharStringCol = None
        NTextStringCol = None 
        GuidCol = None } |> ignore
    let entity = MsSql.find<OptionDefaultMapping> [1]
    printfn "%A" entity
    assert_equal None entity.ByteCol
    assert_equal None entity.Int16Col
    assert_equal None entity.Int32Col
    assert_equal None entity.Int64Col
    assert_equal None entity.BinaryCol
    assert_equal None entity.ImageBinaryCol
    assert_equal None entity.VarbinaryBinaryCol
    assert_true (not <| Array.isEmpty entity.RowversionBinaryCol.Value)
    assert_equal None entity.DateTimeCol 
    assert_equal None entity.DateDateTimeCol
    assert_equal None entity.DateTime2Col 
    assert_equal None entity.SmallDateTimeCol 
    assert_equal None entity.DateTimeOffsetCol
    assert_equal None entity.DecimalCol
    assert_equal None entity.NumericDecimalCol
    assert_equal None entity.MoneyDecimalCol
    assert_equal None entity.SmallMoneyDecimalCol 
    assert_equal None entity.DoubleCol
    assert_equal None entity.SingleCol
    assert_equal None entity.VarcharStringCol 
    assert_equal None entity.NVarcharStringCol 
    assert_equal None entity.NTextStringCol
    assert_equal None entity.GuidCol

  [<Table(Name = "DefaultMapping")>]
  type NullableDefaultMapping =
    { [<Id>]
      DefaultMappingId : int32 
      ByteCol : byte Nullable
      Int16Col : int16 Nullable
      Int32Col : int32 Nullable
      Int64Col : int64 Nullable
      BinaryCol : byte[]
      ImageBinaryCol : byte[]
      VarbinaryBinaryCol : byte[]
      [<Version(VersionKind.Computed)>]
      RowversionBinaryCol : byte[]
      BooleanCol : bool Nullable
      DateTimeCol : DateTime Nullable
      DateDateTimeCol : DateTime Nullable
      DateTime2Col : DateTime Nullable
      SmallDateTimeCol : DateTime Nullable
      DateTimeOffsetCol : DateTimeOffset Nullable
      DecimalCol : decimal Nullable
      NumericDecimalCol : decimal Nullable
      MoneyDecimalCol : decimal Nullable
      SmallMoneyDecimalCol : decimal Nullable
      DoubleCol : double Nullable
      SingleCol : single Nullable
      VarcharStringCol : string
      NVarcharStringCol : string
      NTextStringCol : string
      GuidCol : Guid Nullable }

  [<Test>]
  let ``nullable mapping : not null``() =
    use tx = new TransactionScope ()
    let guid = Guid.NewGuid()
    MsSql.insert<NullableDefaultMapping>
      { DefaultMappingId = 1 
        ByteCol = Nullable 3uy
        Int16Col = Nullable 123s
        Int32Col = Nullable ((int32 Int16.MaxValue) * 2)
        Int64Col = Nullable ((int64 Int32.MaxValue) * 2L)
        BinaryCol = [| 1uy; 2uy; 3uy; |]
        ImageBinaryCol = [| 1uy; 2uy; 3uy; |]
        VarbinaryBinaryCol = [| 1uy; 2uy; 3uy; |]
        RowversionBinaryCol = [| |]
        BooleanCol = Nullable true
        DateTimeCol = Nullable (DateTime(2011, 4, 11, 13, 14, 15))
        DateDateTimeCol = Nullable (DateTime(2011, 4, 11, 13, 14, 15))
        DateTime2Col = Nullable (DateTime(2011, 4, 11, 13, 14, 15))
        SmallDateTimeCol = Nullable (DateTime(2011, 4, 11, 13, 14, 15))
        DateTimeOffsetCol = Nullable (DateTimeOffset(DateTime(2011, 4, 11, 13, 14, 15)))
        DecimalCol = Nullable 123.456M
        NumericDecimalCol = Nullable 123.456M
        MoneyDecimalCol = Nullable 123.456M
        SmallMoneyDecimalCol = Nullable 123.456M
        DoubleCol = Nullable 123.5
        SingleCol = Nullable 123.5f
        VarcharStringCol = "abc"
        NVarcharStringCol = "あいう"
        NTextStringCol = "あいう" 
        GuidCol = Nullable guid } |> ignore
    let entity = MsSql.find<NullableDefaultMapping> [1]
    printfn "%A" entity
    assert_equal (Nullable 3uy) entity.ByteCol
    assert_equal (Nullable 123s) entity.Int16Col
    assert_equal (Nullable ((int32 Int16.MaxValue) * 2)) entity.Int32Col
    assert_equal (Nullable ((int64 Int32.MaxValue) * 2L)) entity.Int64Col
    assert_equal [|1uy; 2uy; 3uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|] entity.BinaryCol
    assert_equal [|1uy; 2uy; 3uy |] entity.ImageBinaryCol
    assert_equal [|1uy; 2uy; 3uy |] entity.VarbinaryBinaryCol
    assert_true (not <| Array.isEmpty entity.RowversionBinaryCol)
    assert_equal (Nullable (DateTime(2011, 4, 11, 13, 14, 15))) entity.DateTimeCol 
    assert_equal (Nullable (DateTime(2011, 4, 11, 0, 0, 0))) entity.DateDateTimeCol
    assert_equal (Nullable (DateTime(2011, 4, 11, 13, 14, 15))) entity.DateTime2Col 
    assert_equal (Nullable (DateTime(2011, 4, 11, 13, 14, 0))) entity.SmallDateTimeCol 
    assert_equal (Nullable (DateTimeOffset(DateTime(2011, 4, 11, 13, 14, 15)))) entity.DateTimeOffsetCol
    assert_equal (Nullable 123.456M) entity.DecimalCol
    assert_equal (Nullable 123.456M) entity.NumericDecimalCol
    assert_equal (Nullable 123.456M) entity.MoneyDecimalCol
    assert_equal (Nullable 123.456M) entity.SmallMoneyDecimalCol 
    assert_equal (Nullable 123.5) entity.DoubleCol
    assert_equal (Nullable 123.5f) entity.SingleCol
    assert_equal "abc" entity.VarcharStringCol 
    assert_equal "あいう" entity.NVarcharStringCol 
    assert_equal "あいう" entity.NTextStringCol
    assert_equal (Nullable guid) entity.GuidCol

  [<Test>]
  let ``nullable mapping : null``() =
    use tx = new TransactionScope ()
    let guid = Guid.NewGuid()
    MsSql.insert<NullableDefaultMapping>
      { DefaultMappingId = 1 
        ByteCol = Nullable ()
        Int16Col = Nullable ()
        Int32Col = Nullable ()
        Int64Col = Nullable ()
        BinaryCol = null
        ImageBinaryCol = null
        VarbinaryBinaryCol = null
        RowversionBinaryCol = null
        BooleanCol = Nullable ()
        DateTimeCol = Nullable ()
        DateDateTimeCol = Nullable ()
        DateTime2Col = Nullable ()
        SmallDateTimeCol = Nullable ()
        DateTimeOffsetCol = Nullable ()
        DecimalCol = Nullable ()
        NumericDecimalCol = Nullable ()
        MoneyDecimalCol = Nullable ()
        SmallMoneyDecimalCol = Nullable ()
        DoubleCol = Nullable ()
        SingleCol = Nullable ()
        VarcharStringCol = null
        NVarcharStringCol = null
        NTextStringCol = null
        GuidCol = Nullable () } |> ignore
    let entity = MsSql.find<NullableDefaultMapping> [1]
    printfn "%A" entity
    assert_equal (Nullable ()) entity.ByteCol
    assert_equal (Nullable ()) entity.Int16Col
    assert_equal (Nullable ()) entity.Int32Col
    assert_equal (Nullable ()) entity.Int64Col
    assert_equal null entity.BinaryCol
    assert_equal null entity.ImageBinaryCol
    assert_equal null entity.VarbinaryBinaryCol
    assert_true (not <| Array.isEmpty entity.RowversionBinaryCol)
    assert_equal (Nullable ()) entity.DateTimeCol 
    assert_equal (Nullable ()) entity.DateDateTimeCol
    assert_equal (Nullable ()) entity.DateTime2Col 
    assert_equal (Nullable ()) entity.SmallDateTimeCol 
    assert_equal (Nullable ()) entity.DateTimeOffsetCol
    assert_equal (Nullable ()) entity.DecimalCol
    assert_equal (Nullable ()) entity.NumericDecimalCol
    assert_equal (Nullable ()) entity.MoneyDecimalCol
    assert_equal (Nullable ()) entity.SmallMoneyDecimalCol 
    assert_equal (Nullable ()) entity.DoubleCol
    assert_equal (Nullable ()) entity.SingleCol
    assert_equal null entity.VarcharStringCol 
    assert_equal null entity.NVarcharStringCol 
    assert_equal null entity.NTextStringCol
    assert_equal (Nullable ()) entity.GuidCol

  type TimeSpanMapping =
    { [<Id>]
      TimeSpanMappingId : int32 
      TimeSpanCol : TimeSpan }

  [<Test>]
  let ``timespan mapping``() =
    use tx = new TransactionScope ()
    MsSql.insert<TimeSpanMapping>
      { TimeSpanMappingId = 1 
        TimeSpanCol = TimeSpan(13, 14, 15) } |> ignore
    let entity = MsSql.find<TimeSpanMapping> [1]
    printfn "%A" entity
    assert_equal (TimeSpan(13, 14, 15)) entity.TimeSpanCol

  [<Table(Name = "TimeSpanMapping")>]
  type OptionTimeSpanMapping =
    { [<Id>]
      TimeSpanMappingId : int32 
      TimeSpanCol : TimeSpan option }

  [<Test>]
  let ``option timespan mapping : some``() =
    use tx = new TransactionScope ()
    MsSql.insert<OptionTimeSpanMapping>
      { TimeSpanMappingId = 1 
        TimeSpanCol = Some (TimeSpan(13, 14, 15)) } |> ignore
    let entity = MsSql.find<OptionTimeSpanMapping> [1]
    printfn "%A" entity
    assert_equal (Some (TimeSpan(13, 14, 15))) entity.TimeSpanCol

  [<Test>]
  let ``option timespan mapping : none``() =
    use tx = new TransactionScope ()
    MsSql.insert<OptionTimeSpanMapping>
      { TimeSpanMappingId = 1 
        TimeSpanCol = None } |> ignore
    let entity = MsSql.find<OptionTimeSpanMapping> [1]
    printfn "%A" entity
    assert_equal None entity.TimeSpanCol

  [<Table(Name = "TimeSpanMapping")>]
  type NullableTimeSpanMapping =
    { [<Id>]
      TimeSpanMappingId : int32 
      TimeSpanCol : TimeSpan Nullable }

  [<Test>]
  let ``nullable timespan mapping : not null``() =
    use tx = new TransactionScope ()
    MsSql.insert<NullableTimeSpanMapping>
      { TimeSpanMappingId = 1 
        TimeSpanCol = Nullable (TimeSpan(13, 14, 15)) } |> ignore
    let entity = MsSql.find<NullableTimeSpanMapping> [1]
    printfn "%A" entity
    assert_equal (Nullable (TimeSpan(13, 14, 15))) entity.TimeSpanCol

  [<Test>]
  let ``nullable timespan mapping : null``() =
    use tx = new TransactionScope ()
    MsSql.insert<NullableTimeSpanMapping>
      { TimeSpanMappingId = 1 
        TimeSpanCol = Nullable () } |> ignore
    let entity = MsSql.find<NullableTimeSpanMapping> [1]
    printfn "%A" entity
    assert_equal (Nullable ()) entity.TimeSpanCol

  type LobMapping =
    { [<Id>]
      LobMappingId : int32 
      VarBinaryCol : byte[]
      VarcharStringCol : string
      NVarcharStringCol : string }

  [<Test>]
  let ``lob mapping``() =
    use tx = new TransactionScope ()
    MsSql.insert<LobMapping>
      { LobMappingId = 1 
        VarBinaryCol = [| 1uy; 2uy; 3uy; |]
        VarcharStringCol = "abc"
        NVarcharStringCol = "あいう" } |> ignore
    let entity = MsSql.find<LobMapping> [1]
    printfn "%A" entity
    assert_equal [|1uy; 2uy; 3uy |] entity.VarBinaryCol
    assert_equal "abc" entity.VarcharStringCol 
    assert_equal "あいう" entity.NVarcharStringCol 

  type CharMapping =
    { [<Id>]
      CharMappingId : int32 
      CharCol : CharString }

  [<Test>]
  let ``char mapping``() =
    use tx = new TransactionScope ()
    MsSql.insert<CharMapping>
      { CharMappingId = 1 
        CharCol = CharString("abc") } |> ignore
    let entity = MsSql.find<CharMapping> [1]
    printfn "%A" entity
    assert_equal (CharString("abc       ")) entity.CharCol
    assert_equal 10 entity.CharCol.Value.Length
    let list = 
      MsSql.query 
        "select * from CharMapping where CharCol = /* CharCol */'' "
        ["CharCol" @= CharString("abc")]
    printfn "%A" list
    assert_equal 1 list.Length
    assert_equal (CharString("abc       ")) list.Head.CharCol

  [<Table(Name = "CharMapping")>]
  type OptionCharMapping =
    { [<Id>]
      CharMappingId : int32 
      CharCol : CharString option}

  [<Test>]
  let ``option char mapping : some``() =
    use tx = new TransactionScope ()
    MsSql.insert<OptionCharMapping>
      { CharMappingId = 1 
        CharCol = Some (CharString("abc")) } |> ignore
    let entity = MsSql.find<OptionCharMapping> [1]
    printfn "%A" entity
    assert_equal (Some (CharString("abc       "))) entity.CharCol
    assert_equal 10 entity.CharCol.Value.Value.Length
    let list = 
      MsSql.query<OptionCharMapping>
        "select * from CharMapping where CharCol = /* CharCol */'' "
        ["CharCol" @= CharString("abc")]
    printfn "%A" list
    assert_equal 1 list.Length
    assert_equal (Some (CharString("abc       "))) list.Head.CharCol

  [<Test>]
  let ``option char mapping : none``() =
    use tx = new TransactionScope ()
    MsSql.insert<OptionCharMapping>
      { CharMappingId = 1 
        CharCol = None } |> ignore
    let entity = MsSql.find<OptionCharMapping> [1]
    printfn "%A" entity
    assert_equal None entity.CharCol
    let list = 
      MsSql.query<OptionCharMapping>
        "select * from CharMapping where CharCol is null"
        []
    printfn "%A" list
    assert_equal 1 list.Length
    assert_equal None list.Head.CharCol