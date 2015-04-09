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

namespace Soma.Core

open System
open System.Reflection
open System.Collections.Generic

type internal SequenceMeta = 
  { SequenceName : string
    SqlSequenceName : string
    Generate : string -> (unit -> obj) -> int64 }

type internal IdCase =
  | Assigned
  | Identity
  | Sequence of SequenceMeta

type internal PropCase =
  | Id of IdCase
  | Version of VersionKind
  | Basic

type internal PropMeta = 
  { Index : int
    PropName : string
    ColumnName : string
    SqlColumnName : string
    IsInsertable : bool
    IsUpdatable : bool
    PropCase : PropCase
    Type : Type
    Property : PropertyInfo
    GetValue : obj -> obj 
    SetValue : obj -> obj -> unit }

type internal PreInsertCase =
  | GetSequenceAndInitVersion of PropMeta * SequenceMeta * PropMeta
  | GetSequence of PropMeta * SequenceMeta
  | InitVersion of PropMeta

type internal InsertCase =
  | InsertThenGetIdentityAndVersionAtOnce of PropMeta * PropMeta
  | InsertThenGetIentityAtOnce of PropMeta
  | InsertThenGetVersionAtOnce of PropMeta
  | InsertThenGetIdentityAndVersionLater of PropMeta * PropMeta
  | InsertThenGetIdentityLater of PropMeta
  | InsertThenGetVersionLater of PropMeta
  | InsertOnly

type internal UpdateCase =
  | UpdateThenGetVersionAtOnce of PropMeta
  | UpdateThenGetVersionLater of PropMeta
  | UpdateThenIncrementVersion of PropMeta
  | UpdateOnly

type internal ValueCase =
  | Changed of obj
  | Unchanged of obj

type internal EntityMeta = 
  { EntityName : string
    TableName : string
    SqlTableName : string
    PropMetaList : PropMeta list
    IdPropMetaList : PropMeta list
    VersionPropMeta : PropMeta option
    Type : Type
    PreInsertCase : PreInsertCase option
    InsertCase : InsertCase
    UpdateCase : UpdateCase
    MakeEntity : obj[] -> obj 
    RemakeEntity : obj -> ValueCase[] -> obj }

type internal BasicElementMeta = 
  { Index : int
    Type : Type }

type internal EntityElementMeta = 
  { Index : int
    EntityMeta : EntityMeta }

type internal TupleMeta = 
  { BasicElementMetaList : BasicElementMeta list
    EntityElementMetaList : EntityElementMeta list
    Type : Type
    MakeTuple : obj[] -> obj}

type internal ResultElementCase =
  | EntityType of EntityMeta
  | TupleType of TupleMeta

type internal ParamMetaCase =
  | Unit
  | Input
  | InputOutput
  | Output
  | ReturnValue
  | Result of ResultElementCase * (seq<obj> -> obj)

type internal ProcedureParamMeta =
  { Index : int
    ParamName : string 
    Type : Type
    ParamMetaCase : ParamMetaCase
    Size : int option
    Precision : byte option
    Scale : byte option
    UdtTypeName : string
    Property : PropertyInfo
    GetValue : obj -> obj 
    SetValue : obj -> obj -> unit }

type internal ProcedureMeta =
  { ProcedureName : string
    SqlProcedureName : string
    ProcedureParamMetaList : ProcedureParamMeta list
    MakeProcedure : obj[] -> obj
    RemakeProcedure : obj -> ValueCase[] -> obj }

type internal PocoMeta =
  { ExtractProperties : obj -> IDictionary<string, obj * Type> }

type internal MetaException =
  inherit InvalidOperationException
  new : message:Message * ?innerException:exn -> MetaException
  member MessageId : string

[<RequireQualifiedAccess>]
module internal Meta =
  val isEntityType : Type -> bool
  val isProcedureType : Type -> bool
  val isPocoType : Type -> bool
  val makeEntityMeta : Type -> IDialect -> EntityMeta
  val makeTupleMeta : Type -> IDialect -> TupleMeta
  val makeProcedureMeta : Type -> IDialect -> ProcedureMeta
  val makePocoMeta : Type -> PocoMeta
