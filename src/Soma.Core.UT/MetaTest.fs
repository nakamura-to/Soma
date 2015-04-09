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

module MetaTest = 

  open System
  open NUnit.Framework
  open Soma.Core

  let dialect = MsSqlDialect() :> IDialect

  type Hoge1 = { Id:int }

  [<Test>]
  let ``makeEntityMeta : propMeta`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge1> dialect
    let propMeta = meta.PropMetaList.[0]
    assert_equal "Id" propMeta.PropName
    assert_equal "Id" propMeta.ColumnName
    assert_equal true propMeta.IsInsertable
    assert_equal true propMeta.IsUpdatable
    assert_equal Basic propMeta.PropCase
    assert_equal typeof<int> propMeta.Type
    assert_equal 10 (propMeta.GetValue (box { Hoge1.Id = 10 }))

  type Hoge2 = { [<Id>]Id:int }

  [<Test>]
  let ``makeEntityMeta : propMeta : IdAttribute`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge2> dialect
    let propMeta = meta.PropMetaList.[0]
    assert_equal "Id" propMeta.PropName
    assert_equal "Id" propMeta.ColumnName
    assert_equal true propMeta.IsInsertable
    assert_equal true propMeta.IsUpdatable
    assert_true  (propMeta.PropCase |> function Id Assigned -> true | _ -> false)
    assert_equal typeof<int> propMeta.Type
    assert_equal 10 (propMeta.GetValue (box { Hoge2.Id = 10 }))

  type Hoge3 = { [<Column(Name = "Aaa", Insertable = false, Updatable = false)>]Bbb:string }

  [<Test>]
  let ``makeEntityMeta : propMeta : ColumnAttribute`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge3> dialect
    let propMeta = meta.PropMetaList.[0]
    assert_equal "Bbb" propMeta.PropName
    assert_equal "Aaa" propMeta.ColumnName
    assert_equal false propMeta.IsInsertable
    assert_equal false propMeta.IsUpdatable
    assert_equal Basic propMeta.PropCase
    assert_equal typeof<string> propMeta.Type
    assert_equal "abc" (propMeta.GetValue (box { Hoge3.Bbb = "abc" }))

  type Hoge4 = { Name:string }

  [<Test>]
  let ``makeEntityMeta : no id`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge4> dialect
    assert_equal "Hoge4" meta.TableName
    assert_equal 1 meta.PropMetaList.Length
    assert_true meta.IdPropMetaList.IsEmpty
    assert_true meta.VersionPropMeta.IsNone

  type Hoge5 = { [<Id>]Id:int }

  [<Test>]
  let ``makeEntityMeta : single id`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge5> dialect
    assert_equal "Hoge5" meta.TableName
    assert_equal 1 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  type Hoge6 = { [<Id>]Id1:int; [<Id>]Id2:int }

  [<Test>]
  let ``makeEntityMeta : multiple id`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge6> dialect
    assert_equal "Hoge6" meta.TableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 2 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  type Hoge7 = { [<Id>]Id:int; [<Version>]Version:int }

  [<Test>]
  let ``makeEntityMeta : version`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge7> dialect
    assert_equal "Hoge7" meta.TableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsSome

  type Hoge13 = { [<Id>]Id:int; [<Version>]Version:string }

  [<Test>]
  let ``makeEntityMeta : version not incremental`` () =
    try
      Meta.makeEntityMeta typeof<Hoge13> dialect |> ignore
      fail ()
    with
    | :? MetaException as ex -> 
      printfn "%A" ex
      assert_equal "SOMA3001" ex.MessageId

  type Hoge14 = { [<Id(IdKind.Identity)>]Id1:int; [<Id(IdKind.Identity)>]Id2:int; [<Version>]Version:int }

  [<Test>]
  let ``makeEntityMeta : multi non-assigned id`` () =
    try
      Meta.makeEntityMeta typeof<Hoge14> dialect |> ignore
      fail ()
    with
    | :? MetaException as ex -> 
      printfn "%A" ex
      assert_equal "SOMA3002" ex.MessageId

  type Hoge15 = { [<Id>]Id1:int; [<Id(IdKind.Identity)>]Id2:int; [<Version>]Version:int }

  [<Test>]
  let ``makeEntityMeta : assdigned id and non-assigned id`` () =
    try
      Meta.makeEntityMeta typeof<Hoge15> dialect |> ignore
      fail ()
    with
    | :? MetaException as ex -> 
      printfn "%A" ex
      assert_equal "SOMA3003" ex.MessageId

  type Hoge16 = { [<Id>]Id:int; [<Version>]Version1:int; [<Version>]Version2:int }

  [<Test>]
  let ``makeEntityMeta : multi version`` () =
    try
      Meta.makeEntityMeta typeof<Hoge16> dialect |> ignore
      fail ()
    with
    | :? MetaException as ex -> 
      printfn "%A" ex
      assert_equal "SOMA3004" ex.MessageId

  type Hoge17 = { [<Id>][<Version>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : both id and version`` () =
    try
      Meta.makeEntityMeta typeof<Hoge17> dialect |> ignore
      fail ()
    with
    | :? MetaException as ex -> 
      printfn "%A" ex
      assert_equal "SOMA3005" ex.MessageId

  [<Table(Catalog = "CATALOG")>]
  type Hoge8 = { [<Id>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : catalog`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge8> dialect
    assert_equal "CATALOG.Hoge8" meta.TableName
    assert_equal "CATALOG.Hoge8" meta.SqlTableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  [<Table(Catalog = "CATALOG", IsEnclosed = true)>]
  type Hoge30 = { [<Id>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : catalog isEnclosed`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge30> dialect
    assert_equal "CATALOG.Hoge30" meta.TableName
    assert_equal "[CATALOG].[Hoge30]" meta.SqlTableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  [<Table(Schema = "SCHEMA")>]
  type Hoge9 = { [<Id>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : schema`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge9> dialect
    assert_equal "SCHEMA.Hoge9" meta.TableName
    assert_equal "SCHEMA.Hoge9" meta.SqlTableName

    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  [<Table(Schema = "SCHEMA", IsEnclosed = true)>]
  type Hoge31 = { [<Id>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : schema isEnclosed`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge31> dialect
    assert_equal "SCHEMA.Hoge31" meta.TableName
    assert_equal "[SCHEMA].[Hoge31]" meta.SqlTableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  [<Table(Name = "TABLE")>]
  type Hoge10 = { [<Id>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : table`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge10> dialect
    assert_equal "TABLE" meta.TableName
    assert_equal "TABLE" meta.SqlTableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  [<Table(Name = "TABLE", IsEnclosed = true)>]
  type Hoge32 = { [<Id>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : table isEnclosed`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge32> dialect
    assert_equal "TABLE" meta.TableName
    assert_equal "[TABLE]" meta.SqlTableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  [<Table(Catalog = "CATALOG", Schema = "SCHEMA", Name = "TABLE")>]
  type Hoge11 = { [<Id>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : catalog schema table`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge11> dialect
    assert_equal "CATALOG.SCHEMA.TABLE" meta.TableName
    assert_equal "CATALOG.SCHEMA.TABLE" meta.SqlTableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  [<Table(Catalog = "CATALOG", Schema = "SCHEMA", Name = "TABLE", IsEnclosed = true)>]
  type Hoge33 = { [<Id>]Id:int; Name:string }

  [<Test>]
  let ``makeEntityMeta : catalog schema table isEnclosed`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge33> dialect
    assert_equal "CATALOG.SCHEMA.TABLE" meta.TableName
    assert_equal "[CATALOG].[SCHEMA].[TABLE]" meta.SqlTableName
    assert_equal 2 meta.PropMetaList.Length
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsNone

  type Hoge12 = { [<Id>][<Column(Name="pk")>]Id:int; [<Version>][<Column(Name="version")>]Version:int }

  [<Test>]
  let ``makeEntityMeta : column`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge12> dialect
    assert_equal "Hoge12" meta.TableName
    assert_equal 2 meta.PropMetaList.Length
    let pkPropMeta = meta.PropMetaList.[0]
    assert_equal "pk" pkPropMeta.ColumnName
    assert_equal "pk" pkPropMeta.SqlColumnName
    let versionPropMeta = meta.PropMetaList.[1]
    assert_equal "version" versionPropMeta.ColumnName
    assert_equal "version" versionPropMeta.SqlColumnName
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsSome

  type Hoge34 = { [<Id>][<Column(Name="pk", IsEnclosed=true)>]Id:int; [<Version>][<Column(Name="version", IsEnclosed=true)>]Version:int }

  [<Test>]
  let ``makeEntityMeta : column isEnclosed`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge34> dialect
    assert_equal "Hoge34" meta.TableName
    assert_equal 2 meta.PropMetaList.Length
    let pkPropMeta = meta.PropMetaList.[0]
    assert_equal "pk" pkPropMeta.ColumnName
    assert_equal "[pk]" pkPropMeta.SqlColumnName
    let versionPropMeta = meta.PropMetaList.[1]
    assert_equal "version" versionPropMeta.ColumnName
    assert_equal "[version]" versionPropMeta.SqlColumnName
    assert_equal 1 meta.IdPropMetaList.Length
    assert_true meta.VersionPropMeta.IsSome

  [<Test>]
  let ``makeEntityMeta`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge1> dialect
    let meta2 = Meta.makeEntityMeta typeof<Hoge1> dialect
    assert_true (obj.ReferenceEquals(meta, meta2))

  type Hoge40 = { [<Id>]Id:int; [<Version>]Version:int; Name :string }

  [<Test>]
  let ``makeEntityMeta : MakeEntity : record`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge40> dialect
    let entity = meta.MakeEntity [| 1; 2; "aaa" |] :?> Hoge40
    assert_equal 1 entity.Id
    assert_equal 2 entity.Version
    assert_equal "aaa" entity.Name

  [<Test>]
  let ``makeEntityMeta : RemakeEntity : record`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge40> dialect
    let original = { Hoge40.Id = 1; Version = 2; Name = "aaa" }
    let entity = meta.RemakeEntity (box original) [| Changed 3; Changed 4; Unchanged "aaa" |] :?> Hoge40
    assert_false (obj.ReferenceEquals(original, entity))
    assert_equal 3 entity.Id
    assert_equal 4 entity.Version
    assert_equal "aaa" "aaa"

  type Hoge41() = 
    let mutable id:int = 0
    let mutable version:int = 0
    let mutable name:string = null
    [<Id>]
    member this.Id
      with get () = id
      and  set (v) = id <- v
    [<Version>]
    member this.Version
      with get () = version
      and  set (v) = version <- v
    member this.Name
      with get () = name
      and  set (v) = name <- v

  [<Test>]
  let ``makeEntityMeta : MakeEntity : class`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge41> dialect
    let entity = meta.MakeEntity [| 1; 2; "aaa" |] :?> Hoge41
    assert_equal 1 entity.Id
    assert_equal 2 entity.Version
    assert_equal "aaa" entity.Name

  [<Test>]
  let ``makeEntityMeta : RemakeEntity : class`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge41> dialect
    let original = Hoge41(Id = 1, Version = 2, Name = "aaa")
    let entity = meta.RemakeEntity (box original) [| Changed 3; Changed 4; Unchanged "bbb" |] :?> Hoge41
    assert_true (obj.ReferenceEquals(original, entity))
    assert_equal 3 entity.Id
    assert_equal 4 entity.Version
    assert_equal "aaa" entity.Name

  type Hoge60 = { [<Id(IdKind.Sequence)>][<Sequence>]Id:int; [<Version>]Version:int }

  [<Test>]
  let ``makeEntityMeta : sequence`` () =
    let entityMeta = Meta.makeEntityMeta typeof<Hoge60> dialect
    let propertyMeta = entityMeta.IdPropMetaList.[0]
    match propertyMeta.PropCase with
    | Id (Sequence (sequenceMeta)) -> 
      assert_equal "Hoge60_SEQ" sequenceMeta.SequenceName
    | _ -> 
      fail ()

  type Hoge61 = { [<Id(IdKind.Sequence)>][<Sequence(Catalog = "CATALOG", Schema = "SCHEMA", Name = "SEQUENCE")>]Id:int; [<Version>]Version:int }

  [<Test>]
  let ``makeEntityMeta : sequence : catalog schema name`` () =
    let entityMeta = Meta.makeEntityMeta typeof<Hoge61> dialect
    let propertyMeta = entityMeta.IdPropMetaList.[0]
    match propertyMeta.PropCase with
    | Id (Sequence (sequenceMeta)) -> 
      assert_equal "CATALOG.SCHEMA.SEQUENCE" sequenceMeta.SequenceName
    | _ -> 
      fail ()

  type Hoge62 = { [<Id(IdKind.Sequence)>]Id:int; [<Version>]Version:int }

  [<Test>]
  let ``makeEntityMeta : sequence not found`` () =
    try 
      Meta.makeEntityMeta typeof<Hoge62> dialect |> ignore
      fail ()
    with
    | :? MetaException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA3009" ex.MessageId
    | ex -> 
      fail ex

  [<Table(Name = "FOO")>]
  type Hoge63 = { [<Id(IdKind.Sequence)>][<Sequence>]Id:int; [<Version>]Version:int }

  [<Test>]
  let ``makeEntityMeta : sequence : use tableName`` () =
    let entityMeta = Meta.makeEntityMeta typeof<Hoge63> dialect
    let propertyMeta = entityMeta.IdPropMetaList.[0]
    match propertyMeta.PropCase with
    | Id (Sequence (sequenceMeta)) -> 
      assert_equal "FOO_SEQ" sequenceMeta.SequenceName
    | _ -> 
      fail ()

  [<Test>]
  let ``makeTupleMeta`` () =
    let meta = Meta.makeTupleMeta typeof<int * string * Hoge12> dialect
    assert_equal 2 meta.BasicElementMetaList.Length
    assert_equal 1 meta.EntityElementMetaList.Length

  [<Test>]
  let ``makeTupleMeta : record index is invalid`` () =
    try
      Meta.makeTupleMeta typeof<Hoge12 * int> dialect |> ignore
      fail ()
    with 
    | :? MetaException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA3000" ex.MessageId
    | ex -> 
      fail ex

  type Hoge20 = { Param1 : int; Param2 : string }

  [<Test>]
  let ``makeProcedureMeta`` () =
    let meta = Meta.makeProcedureMeta typeof<Hoge20> dialect
    assert_equal "Hoge20" meta.ProcedureName
    assert_equal "Hoge20" meta.SqlProcedureName
    assert_equal 2 meta.ProcedureParamMetaList.Length

  [<Procedure(IsEnclosed = true)>]
  type Hoge35 = { Param1 : int; Param2 : string }

  [<Test>]
  let ``makeProcedureMeta isEnclosed`` () =
    let meta = Meta.makeProcedureMeta typeof<Hoge35> dialect
    assert_equal "Hoge35" meta.ProcedureName
    assert_equal "[Hoge35]" meta.SqlProcedureName
    assert_equal 2 meta.ProcedureParamMetaList.Length

  type Hoge21 = { Param1 : int; [<ProcedureParam(Direction = Direction.Result)>]Param2 : string }

  [<Test>]
  let ``makeProcedureMeta : illegal result type`` () =
    try
      Meta.makeProcedureMeta typeof<Hoge21> dialect |> ignore
      fail ()
    with
    | :? MetaException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA3006" ex.MessageId
    | ex -> 
      fail ex

  type Hoge22 = { [<ProcedureParam(Direction = Direction.ReturnValue)>]Param1 : int; [<ProcedureParam(Direction = Direction.ReturnValue)>]Param2 : string }

  [<Test>]
  let ``makeProcedureMeta : 2 ReturnValue parameters.`` () =
    try
      Meta.makeProcedureMeta typeof<Hoge22> dialect |> ignore
      fail ()
    with
    | :? MetaException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA3008" ex.MessageId
    | ex -> 
      fail ex

  type Hoge50 = { Param1:int; Param2:int; Param3:string }

  [<Test>]
  let ``makeProcedureMeta : MakeProcedure : record`` () =
    let meta = Meta.makeProcedureMeta typeof<Hoge50> dialect
    let procedure = meta.MakeProcedure [| 1; 2; "aaa" |] :?> Hoge50
    assert_equal 1 procedure.Param1
    assert_equal 2 procedure.Param2

  [<Test>]
  let ``makeProcedureMeta : RemakeProcedure : record`` () =
    let meta = Meta.makeProcedureMeta typeof<Hoge50> dialect
    let original = { Hoge50.Param1 = 1; Param2 = 2; Param3 = "aaa" }
    let procedure = meta.RemakeProcedure (box original) [| Changed 3; Changed 4; Unchanged "aaa" |] :?> Hoge50
    assert_false (obj.ReferenceEquals(original, procedure))
    assert_equal 3 procedure.Param1
    assert_equal 4 procedure.Param2
    assert_equal "aaa" procedure.Param3

  type Hoge51() = 
    let mutable param1:int = 0
    let mutable param2:int = 0
    let mutable param3:string = null
    [<Id>]
    member this.Param1
      with get () = param1
      and  set (v) = param1 <- v
    [<Version>]
    member this.Param2
      with get () = param2
      and  set (v) = param2 <- v
    member this.Param3
      with get () = param3
      and  set (v) = param3 <- v

  [<Test>]
  let ``makeProcedureMeta : MakeProcedure : class`` () =
    let meta = Meta.makeProcedureMeta typeof<Hoge51> dialect
    let procedure = meta.MakeProcedure [| 1; 2; "aaa" |] :?> Hoge51
    assert_equal 1 procedure.Param1
    assert_equal 2 procedure.Param2

  [<Test>]
  let ``makeProcedureMeta : RemakeProcedure : class`` () =
    let meta = Meta.makeProcedureMeta typeof<Hoge51> dialect
    let original = Hoge51(Param1 = 1, Param2 = 2, Param3 = "aaa")
    let procedure = meta.RemakeProcedure (box original) [| Changed 3; Changed 4; Unchanged "bbb" |] :?> Hoge51
    assert_true (obj.ReferenceEquals(original, procedure))
    assert_equal 3 procedure.Param1
    assert_equal 4 procedure.Param2
    assert_equal "aaa" procedure.Param3
 
  type Hoge70 = { Id:int; Name:string }

  type Hoge71() = 
    let id = 0

  [<Test>]
  let ``isEntityType`` () =
    assert_true <| Meta.isEntityType typeof<Hoge70>
    assert_true <| Meta.isEntityType typeof<Hoge71>
    assert_false <| Meta.isEntityType typeof<string>
    assert_false <| Meta.isEntityType typeof<string option>
    assert_false <| Meta.isEntityType typeof<int Nullable>

  [<Test>]
  let ``isProcedureType`` () =
    assert_true <| Meta.isProcedureType typeof<Hoge70>
    assert_true <| Meta.isProcedureType typeof<Hoge71>
    assert_false <| Meta.isProcedureType typeof<string>
    assert_false <| Meta.isProcedureType typeof<string option>
    assert_false <| Meta.isProcedureType typeof<int Nullable>