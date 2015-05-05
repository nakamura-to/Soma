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

type SequenceMeta = 
  { SequenceName : string
    SqlSequenceName : string
    Generate : string -> (unit -> obj) -> int64 }

type IdCase =
  | Assigned
  | Identity
  | Sequence of SequenceMeta

type PropCase =
  | Id of IdCase
  | Version of VersionKind
  | Basic

type PropMeta = 
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

type PreInsertCase =
  | GetSequenceAndInitVersion of PropMeta * SequenceMeta * PropMeta
  | GetSequence of PropMeta * SequenceMeta
  | InitVersion of PropMeta

type InsertCase =
  | InsertThenGetIdentityAndVersionAtOnce of PropMeta * PropMeta
  | InsertThenGetIdentityAtOnce of PropMeta
  | InsertThenGetVersionAtOnce of PropMeta
  | InsertThenGetIdentityAndVersionLater of PropMeta * PropMeta
  | InsertThenGetIdentityLater of PropMeta
  | InsertThenGetVersionLater of PropMeta
  | InsertOnly

type UpdateCase =
  | UpdateThenGetVersionAtOnce of PropMeta
  | UpdateThenGetVersionLater of PropMeta
  | UpdateThenIncrementVersion of PropMeta
  | UpdateOnly

type ValueCase =
  | Changed of obj
  | Unchanged of obj

type EntityMeta = 
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
    InsertOrUpdateCase : InsertCase
    MakeEntity : obj[] -> obj 
    RemakeEntity : obj -> ValueCase[] -> obj }

type BasicElementMeta = 
  { Index : int
    Type : Type }

type EntityElementMeta = 
  { Index : int
    EntityMeta : EntityMeta }

type TupleMeta = 
  { BasicElementMetaList : BasicElementMeta list
    EntityElementMetaList : EntityElementMeta list
    Type : Type
    MakeTuple : obj[] -> obj }

type ResultElementCase =
  | EntityType of EntityMeta
  | TupleType of TupleMeta

type ParamMetaCase =
  | Unit
  | Input
  | InputOutput
  | Output
  | ReturnValue
  | Result of ResultElementCase * (seq<obj> -> obj)

type ProcedureParamMeta =
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

type ProcedureMeta =
  { ProcedureName : string
    SqlProcedureName : string
    ProcedureParamMetaList : ProcedureParamMeta list
    MakeProcedure : obj[] -> obj 
    RemakeProcedure : obj -> ValueCase[] -> obj }

type PocoMeta =
  { ExtractProperties : obj -> IDictionary<string, obj * Type> }

type MetaException (message:Message, ?innerException:exn) =
  inherit InvalidOperationException (message.Format (), match innerException with Some ex -> ex | _ -> null)
  member this.MessageId = message.Id

[<RequireQualifiedAccess>]
module Meta =

  open System.Collections.Generic
  open System.Reflection
  open System.Collections.Concurrent
  open Microsoft.FSharp.Reflection

  let (|IdentityId|_|) (idPropMetaList:PropMeta list) =
    if idPropMetaList.Length = 1 then
      let idPropMeta = idPropMetaList.Head
      match idPropMeta.PropCase with
      | Id Identity ->
        Some idPropMeta
      | _ ->
        None
    else
      None

  let (|SequenceId|_|) (idPropMetaList:PropMeta list) =
    if idPropMetaList.Length = 1 then
      let idPropMeta = idPropMetaList.Head
      match idPropMeta.PropCase with
      | Id (Sequence (sequenceMeta)) ->
        Some (idPropMeta, sequenceMeta)
      | _ ->
        None
    else
      None

  let (|IncrementedVersion|_|) (propMeta:PropMeta option) =
    propMeta
    |> Option.bind (fun propMeta -> 
      match propMeta.PropCase with
      | Version VersionKind.Incremented -> Some propMeta 
      | _ -> None)

  let (|ComputedVersion|_|) (propMeta:PropMeta option) =
    propMeta
    |> Option.bind (fun propMeta -> 
      match propMeta.PropCase with
      | Version VersionKind.Computed -> Some propMeta 
      | _ -> None)

  let mscorlibAssembly = typeof<string>.Assembly

  let fsharpAssembly = typeof<FSharpType>.Assembly

  let isUserDefinedClass (typ:Type) =
    if typ.IsClass && not typ.IsArray then
      let assembly = typ.Assembly
      assembly <> mscorlibAssembly && assembly <> fsharpAssembly
    else 
      false

  let entityMetaCache = ConcurrentDictionary<Type, Lazy<EntityMeta>>()

  let isEntityType typ =
    entityMetaCache.ContainsKey typ
    ||
    FSharpType.IsRecord typ 
    || 
    isUserDefinedClass typ

  let procedureMetaCache = ConcurrentDictionary<Type, Lazy<ProcedureMeta>>()

  let isProcedureType typ =
    procedureMetaCache.ContainsKey typ
    || 
    FSharpType.IsRecord typ 
    || 
    isUserDefinedClass typ

  let pocoMetaCache = ConcurrentDictionary<Type, Lazy<PocoMeta>>()

  let isPocoType typ =
    pocoMetaCache.ContainsKey typ
    || 
    isUserDefinedClass typ

  let getPropertyInfoMetaList typ metaMaker =
    if FSharpType.IsRecord typ then
      FSharpType.GetRecordFields(typ)
      |> Seq.mapi (fun i field -> 
        metaMaker i field (FSharpValue.PreComputeRecordFieldReader field) (fun _ _ -> ()))
      |> Seq.toList
    else 
      typ.GetProperties()
      |> Seq.mapi (fun i prop -> 
        metaMaker i prop (fun obj -> prop.GetValue(obj, null)) (fun obj value -> prop.SetValue(obj, value, null) ))
      |> Seq.toList

  let getInstanceMaker typ (setters) =
    if FSharpType.IsRecord typ then 
      FSharpValue.PreComputeRecordConstructor typ 
    else
      let ctor = typ.GetConstructor [||]
      if ctor = null then 
        raise <| MetaException(SR.SOMA3007 typ.FullName)
      (fun propValues -> 
        let instance = ctor.Invoke [||]
        setters 
        |> Array.iteri (fun i setter -> 
          setter instance propValues.[i])
        instance)

  let getInstanceRemaker typ (setters) =
    if FSharpType.IsRecord typ then 
      (fun originalInstance propValueCases ->
        let values = propValueCases |> Array.map (function Changed v | Unchanged v -> v)
        FSharpValue.PreComputeRecordConstructor typ values)
    else
      (fun originalInstance propValueCases -> 
        setters 
        |> Array.iteri (fun i setter -> 
          match propValueCases.[i] with
          | Changed v -> setter originalInstance v
          | _ -> () )
        originalInstance )

  let inline getDbObjectName (attr: ^a) defaultName (dialect:IDialect) =
    let catalog = (^a : (member Catalog: string) attr)
    let schema = (^a : (member Schema: string) attr)
    let name = (^a : (member Name: string) attr)
    let simpleName = if name <> null then name else defaultName
    let nameList =
      [ asOption catalog; asOption schema; Some simpleName ]
      |> List.choose id
    let concat x y = x + "." + y
    let fullName = nameList |> List.reduce concat
    let enclosedFullName = nameList |> List.map dialect.EncloseIdentifier |> List.reduce concat
    fullName, enclosedFullName, simpleName

  let newSequenceMeta simpleTableName dialect (prop:PropertyInfo) =
    let sequenceName, enclosedSequenceName, isEnclosed, incrementBy =
      match Attribute.GetCustomAttribute(prop, typeof<SequenceAttribute>) with 
      | :? SequenceAttribute as attr ->
        let defaultName = simpleTableName + "_SEQ"
        let sequenceName, enclosedSequenceName, _ = getDbObjectName attr defaultName dialect
        sequenceName, enclosedSequenceName, attr.IsEnclosed, int64 attr.IncrementBy
      | _ -> 
        raise <| MetaException(SR.SOMA3009 (prop.Name, prop.DeclaringType.FullName))
    let baseRef = ref 0L
    let stepRef = ref Int64.MaxValue 
    let generateExclusive (lockObj:obj) (executor:unit -> obj) = 
      lock (lockObj) (fun () ->
        let step = !stepRef
        if step < incrementBy then
          stepRef := step + 1L
          !baseRef + step
        else 
          let value = executor ()
          let ``base`` = Convert.ChangeType(value, typeof<int64>) :?> int64
          baseRef := ``base``
          stepRef := 1L
          ``base`` )
    let contextCache = ConcurrentDictionary<string, (unit -> obj) -> int64>()
    let generator contextKey (executor:unit -> obj) =
      let context = contextCache.GetOrAdd(contextKey, generateExclusive (obj()))
      context executor 
    { SequenceName = sequenceName 
      SqlSequenceName = if isEnclosed then enclosedSequenceName else sequenceName
      Generate = generator }

  let newPropMeta simpleTableName dialect index (prop:PropertyInfo) propReader propWriter =
    let columnName, isInsertable, isUpdatable, isEnclosed = 
      match Attribute.GetCustomAttribute(prop, typeof<ColumnAttribute>) with 
      | :? ColumnAttribute as attr ->
        let name = if attr.Name <> null then attr.Name else prop.Name
        name, attr.Insertable, attr.Updatable, attr.IsEnclosed
      | _ -> 
        prop.Name, true, true, false
    let isNumber = Reflection.isNumberType prop.PropertyType
    let typ = prop.PropertyType 
    let propCase =
      match Attribute.GetCustomAttribute(prop, typeof<IdAttribute>) with 
      | :? IdAttribute as attr ->
        match Attribute.GetCustomAttribute(prop, typeof<VersionAttribute>) with 
        | :? VersionAttribute -> 
          raise <| MetaException(SR.SOMA3005 (prop.Name, prop.DeclaringType.FullName))
        | _ -> 
          match attr.Kind with
          | IdKind.Assigned -> Id Assigned
          | IdKind.Identity -> Id Identity
          | IdKind.Sequence -> Id (Sequence (newSequenceMeta simpleTableName dialect prop))
          | _ -> failwith "unreachable."
      | _ ->
        match Attribute.GetCustomAttribute(prop, typeof<VersionAttribute>) with 
        | :? VersionAttribute as attr ->
          if attr.Kind = VersionKind.Incremented && not isNumber then
            raise <| MetaException(SR.SOMA3001 (prop.Name, prop.DeclaringType.FullName, prop.PropertyType.FullName))
            failwith ""
          PropCase.Version attr.Kind
        | _ ->
          PropCase.Basic
    { Index = index
      PropName = prop.Name
      ColumnName = columnName
      SqlColumnName = if isEnclosed then dialect.EncloseIdentifier columnName else columnName
      IsInsertable = isInsertable
      IsUpdatable = isUpdatable
      PropCase = propCase
      Type = typ
      Property = prop
      GetValue = propReader
      SetValue = propWriter }

  let getPreInsertCase (dialect:IDialect) = function
    | SequenceId (idPropMeta, sequenceMeta), IncrementedVersion versionPropMeta ->
      Some (GetSequenceAndInitVersion (idPropMeta, sequenceMeta, versionPropMeta))
    | SequenceId (idPropMeta, sequenceMeta), _ ->
      Some (GetSequence (idPropMeta, sequenceMeta))
    | _, IncrementedVersion versionPropMeta ->
      Some (InitVersion versionPropMeta)
    | _ ->
      None

  let getInsertCase (dialect:IDialect) = function
    | IdentityId idPropMeta, ComputedVersion versionPropMeta ->
      if dialect.CanGetIdentityAndVersionAtOnce then
        InsertThenGetIdentityAndVersionAtOnce (idPropMeta, versionPropMeta)
      else
        InsertThenGetIdentityAndVersionLater (idPropMeta, versionPropMeta)
    | IdentityId idPropMeta, _ ->
      if dialect.CanGetIdentityAtOnce then
        InsertThenGetIdentityAtOnce idPropMeta
      else
        InsertThenGetIdentityLater idPropMeta
    | _, ComputedVersion versionPropMeta ->
      if dialect.CanGetVersionAtOnce then
        InsertThenGetVersionAtOnce versionPropMeta
      else
        InsertThenGetVersionLater versionPropMeta
    | _ ->
      InsertOnly

  let getUpdateCase (dialect:IDialect) = function
    | ComputedVersion versionPropMeta ->
      if dialect.CanGetVersionAtOnce then
        UpdateThenGetVersionAtOnce versionPropMeta
      else
        UpdateThenGetVersionLater versionPropMeta
    | IncrementedVersion versionPropMeta -> 
      UpdateThenIncrementVersion versionPropMeta
    | _ ->
      UpdateOnly

  let getInsertOrUpdateCase (dialect:IDialect) idPropMeta versionPropMeta =
    let getVersion versionPropMeta =
        if dialect.CanGetVersionAtOnce then
            InsertThenGetVersionAtOnce versionPropMeta
        else
            InsertThenGetVersionLater versionPropMeta
    let getIdentityAndVersion idPropMeta versionPropMeta =
        if dialect.CanGetIdentityAndVersionAtOnce then
            InsertThenGetIdentityAndVersionAtOnce (idPropMeta, versionPropMeta)
        else
            InsertThenGetIdentityAndVersionLater (idPropMeta, versionPropMeta)

    match idPropMeta, versionPropMeta with
        | IdentityId idPropMeta, ComputedVersion versionPropMeta ->
          getIdentityAndVersion idPropMeta versionPropMeta
        | IdentityId idPropMeta, IncrementedVersion versionPropMeta ->
          getIdentityAndVersion idPropMeta versionPropMeta
        | IdentityId idPropMeta, _ ->
          if dialect.CanGetIdentityAtOnce then
            InsertThenGetIdentityAtOnce idPropMeta
          else
            InsertThenGetIdentityLater idPropMeta
        | _, ComputedVersion versionPropMeta ->
            getVersion versionPropMeta
        | _, IncrementedVersion versionPropMeta ->
            getVersion versionPropMeta
        | _ ->
          InsertOnly

  let newEntityMeta dialect (typ:Type) =
    let tableName, enclosedTableName, isEnclosed, simpleTableName =
      match Attribute.GetCustomAttribute(typ, typeof<TableAttribute>) with 
      | :? TableAttribute as attr ->
        let tableName, enclosedTableName, simpleName = getDbObjectName attr typ.Name dialect
        tableName, enclosedTableName, attr.IsEnclosed, simpleName
      | _ -> 
        let typeName = typ.Name
        typeName, dialect.EncloseIdentifier typeName, false, typeName
    let propMetaList = getPropertyInfoMetaList typ (newPropMeta simpleTableName dialect)
    let idPropMetaList =
        propMetaList
        |> List.filter (fun propMeta -> match propMeta.PropCase with Id _ -> true | _ -> false)
        |> List.partition (fun propMeta -> match propMeta.PropCase with Id(Assigned) -> true | _ -> false)
        |> function
            | [], [] -> List.empty
            | [], [ idMeta ] -> [ idMeta ]
            | [], _ -> raise <| MetaException(SR.SOMA3002 (typ.FullName))
            | idMetaList, [] -> idMetaList
            | _, _ -> raise <| MetaException(SR.SOMA3003 (typ.FullName))
    let versionPropMeta = 
      propMetaList
      |> List.filter (fun propMeta -> match propMeta.PropCase with Version _ -> true | _ -> false)
      |> function 
          | [] -> None
          | [ versionMeta ] -> Some versionMeta
          | _ -> raise <| MetaException(SR.SOMA3004 (typ.FullName))
    let preInsertCase = getPreInsertCase dialect (idPropMetaList, versionPropMeta)
    let insertCase = getInsertCase dialect (idPropMetaList, versionPropMeta)
    let updateCase = getUpdateCase dialect versionPropMeta
    let insertOrUpdateCase = getInsertOrUpdateCase dialect idPropMetaList versionPropMeta
    let setters = Array.zeroCreate propMetaList.Length
    propMetaList |> List.iteri (fun i p -> setters.[i] <- p.SetValue )
    let maker = getInstanceMaker typ setters
    let remaker = getInstanceRemaker typ setters
    { EntityName = typ.Name
      TableName = tableName
      SqlTableName = if isEnclosed then enclosedTableName else tableName
      PropMetaList = propMetaList 
      IdPropMetaList = idPropMetaList
      VersionPropMeta = versionPropMeta
      Type = typ
      PreInsertCase = preInsertCase
      InsertCase = insertCase
      UpdateCase = updateCase
      InsertOrUpdateCase = insertOrUpdateCase
      MakeEntity = maker
      RemakeEntity = remaker }
  
  let makeEntityMeta typ dialect =
    entityMetaCache.GetOrAdd(typ, Lazy(fun () -> newEntityMeta dialect typ)).Value

  let newTupleMeta dialect typ = 
    let elements = FSharpType.GetTupleElements(typ)
    let (entityTypeAry, basicTypeAry) =
      elements
      |> Array.mapi (fun i typ -> (i, typ))
      |> Array.partition (fun (i, typ) -> isEntityType typ)
    let basicElmtMetaAry =
      basicTypeAry
      |> Array.map (fun (i, typ) -> { BasicElementMeta.Index = i; Type = typ })
    let entityElmtMetaAry = 
      entityTypeAry
      |> Array.map (fun (i, typ) -> { EntityElementMeta.Index = i; EntityMeta = makeEntityMeta typ dialect })
    if not <| Array.isEmpty entityElmtMetaAry && not <| Array.isEmpty basicElmtMetaAry then
      let basicLastIndex = Array.max (basicElmtMetaAry |> Array.map (fun elmt -> elmt.Index))
      let entityFirstIndex = Array.min (entityElmtMetaAry |> Array.map (fun elmt -> elmt.Index))
      if basicLastIndex > entityFirstIndex then
        let basicTypeName = elements.[basicLastIndex].FullName
        let entityTypeName = elements.[entityFirstIndex].FullName
        raise <| MetaException (SR.SOMA3000 (entityTypeName, basicTypeName, typ.FullName))
    { BasicElementMetaList = Array.toList basicElmtMetaAry
      EntityElementMetaList = Array.toList entityElmtMetaAry
      Type = typ
      MakeTuple = FSharpValue.PreComputeTupleConstructor typ }

  let tupleMetaCache = ConcurrentDictionary<Type, Lazy<TupleMeta>>()
  
  let makeTupleMeta typ dialect =
    tupleMetaCache.GetOrAdd(typ, Lazy(fun () -> newTupleMeta dialect typ)).Value

  let newProcedureParamMeta encloser index (prop:PropertyInfo) propReader propWriter =
    let (|FSharpList|_|) (typ:Type) =
      if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<list<_>> then
        Some <| typ.GetGenericArguments().[0]
      else 
        None
    let (|IList|_|) (typ:Type) =
      [| typ |]
      |> Array.append (typ.GetInterfaces())
      |> Seq.tryFind (fun interfase -> 
        interfase.IsGenericType && interfase.GetGenericTypeDefinition() = typedefof<IList<_>>)
      |> Option.map (fun interfase -> interfase.GetGenericArguments().[0] )
    let getElementCase typ =
      if isEntityType typ then
        EntityType (makeEntityMeta typ encloser)
      else 
        TupleType (makeTupleMeta typ encloser)
    let (paramName, direction, size, precision, scale, udtTypeName) = 
      match Attribute.GetCustomAttribute(prop, typeof<ProcedureParamAttribute>) with 
      | :? ProcedureParamAttribute as attr ->
        let name = if attr.Name <> null then attr.Name else prop.Name
        name, attr.Direction, attr.SizeOpt, attr.PrecisionOpt, attr.ScaleOpt, attr.UdtTypeName
      | _ -> 
        prop.Name, Direction.Input, None, None, None, null
    let typ = prop.PropertyType
    let paramMetaCase =
      if typ = typeof<Unit> then 
        Unit 
      else
        match direction with
        | Direction.Input -> Input
        | Direction.InputOutput -> InputOutput
        | Direction.Output -> Output
        | Direction.ReturnValue -> ReturnValue
        | Direction.Result ->
          Result
          <| match typ with
             | FSharpList elementType ->
               (getElementCase elementType), (Reflection.changeTypeFromSeqToList elementType)
             | IList elementType ->
               (getElementCase elementType), (Reflection.changeTypeFromSeqToResizeArray elementType)
             | _ ->
              raise <| MetaException(SR.SOMA3006 (prop.PropertyType.FullName, prop.Name))
        | _ -> failwith "unreachable."
    { Index = index
      ParamName = paramName
      Type = typ
      ParamMetaCase = paramMetaCase
      Size = size
      Precision = precision
      Scale = scale
      UdtTypeName = udtTypeName
      Property = prop
      GetValue = propReader
      SetValue = propWriter }

  let newProcedureMeta dialect (typ:Type) = 
    let procedureName, enclosedProcedureName, isEnclosed = 
      match Attribute.GetCustomAttribute(typ, typeof<ProcedureAttribute>) with 
      | :? ProcedureAttribute as attr ->
        let procedureName, enclosedProcedureName, _ = getDbObjectName attr typ.Name dialect
        procedureName, enclosedProcedureName, attr.IsEnclosed
      | _ -> 
        typ.Name, dialect.EncloseIdentifier typ.Name, false
    let procedureParamMetaList = getPropertyInfoMetaList typ (newProcedureParamMeta dialect)
    let returnValueParamMetaCount =
      procedureParamMetaList
      |> Seq.filter (fun p -> match p.ParamMetaCase with ReturnValue -> true | _ -> false )
      |> Seq.length
    if returnValueParamMetaCount > 1 then 
      raise <| MetaException(SR.SOMA3008 typ.FullName)
    let setters = Array.zeroCreate procedureParamMetaList.Length
    procedureParamMetaList |> List.iteri (fun i p -> setters.[i] <- p.SetValue )
    let maker = getInstanceMaker typ setters
    let remaker = getInstanceRemaker typ setters
    { ProcedureName = procedureName
      SqlProcedureName = if isEnclosed then enclosedProcedureName else procedureName
      ProcedureParamMetaList = procedureParamMetaList
      MakeProcedure = maker 
      RemakeProcedure = remaker }

  let makeProcedureMeta typ dialect =
    procedureMetaCache.GetOrAdd(typ, Lazy(fun () -> newProcedureMeta dialect typ)).Value

  let newPocoMeta (typ:Type) =
    let props = 
      typ.GetProperties()
      |> Array.filter (fun prop -> prop.CanRead)
      |> Array.map (fun prop -> prop, prop.Name, prop.PropertyType)
    let extractor (obj:obj) =
      props
      |> Seq.map (fun (prop, propName, propType) -> 
         propName, (prop.GetValue(obj, null), propType) )
      |> dict
    { ExtractProperties = extractor }

  let makePocoMeta typ =
    pocoMetaCache.GetOrAdd(typ, Lazy(fun () -> newPocoMeta typ)).Value
