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
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic
open System.ComponentModel
open System.Data
open System.Data.Common
open System.Dynamic
open System.Runtime.InteropServices
open System.Text
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text.Lexing

exception NoAffectedRowException of PreparedStatement with
  override this.Message =
    match this :> exn with
    | NoAffectedRowException(ps) -> 
      let message = SR.SOMA4011 (ps.Text, ps.Parameters)
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

  member this.PreparedStatement =
    match this :> exn with
    | NoAffectedRowException(ps) -> 
      ps
    | _ -> 
      Unchecked.defaultof<_>

exception OptimisticLockException of PreparedStatement with
  override this.Message =
    match this :> exn with
    | OptimisticLockException(ps) -> 
      let message = SR.SOMA4013 (ps.Text, ps.Parameters)
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

  member this.PreparedStatement =
    match this :> exn with
    | OptimisticLockException(ps) -> 
      ps
    | _ -> 
      Unchecked.defaultof<_>

exception UniqueConstraintException of PreparedStatement * string with
  override this.Message =
    match this :> exn with
    | UniqueConstraintException(ps, message) -> 
      let message = SR.SOMA4014 (message, ps.Text, ps.Parameters)
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

  member this.PreparedStatement =
    match this :> exn with
    | UniqueConstraintException(ps, _) -> 
      ps 
    | _ -> 
      Unchecked.defaultof<_>

exception EntityNotFoundException of PreparedStatement with
  override this.Message =
    match this :> exn with
    | EntityNotFoundException(ps) -> 
      let message = SR.SOMA4015 (ps.Text, ps.Parameters)
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

  member this.PreparedStatement =
    match this :> exn with
    | EntityNotFoundException(ps) -> 
      ps
    | _ -> 
      Unchecked.defaultof<_>

[<AbstractClass>]
type DbConfigBase(invariant:string) = 
  do Guard.argNotNull(invariant, "invariant")
  let dbProviderFactory = DbProviderFactories.GetFactory(invariant)
  let statementCache = ConcurrentDictionary<string, Lazy<SqlAst.Statement>>()
  let expressionCache = ConcurrentDictionary<string, Lazy<ExpressionAst.Expression>>()
  let connectionObserver = 
    { new IConnectionObserver with
      member this.NotifyOpening(connection:DbConnection, [<System.Runtime.InteropServices.Out>]userState:byref<obj>) = ()
      member this.NotifyOpened(connection:DbConnection, userState:obj) = () }
  let commandObserver = 
    { new ICommandObserver with
      member this.NotifyExecuting(command:DbCommand, statement:PreparedStatement, [<System.Runtime.InteropServices.Out>]userState:byref<obj>) = ()
      member this.NotifyExecuted(command:DbCommand, statement:PreparedStatement, userState:obj) = () }
  member this.CacheSqlParser = Func<string, SqlAst.Statement>(fun sql -> statementCache.GetOrAdd(sql, Lazy(fun () -> Sql.parse sql)).Value)
  member this.NoCacheSqlParser = Func<string, SqlAst.Statement>(fun sql -> Sql.parse sql)
  member this.CacheExpressionParser = Func<string, ExpressionAst.Expression>(fun expr -> expressionCache.GetOrAdd(expr, Lazy(fun () -> Expression.parse expr)).Value)
  member this.NoCacheExpressionParser = Func<string, ExpressionAst.Expression>(fun expr -> Expression.parse expr)
  member this.ConsoleLogger = Action<PreparedStatement>(fun preparedStatement -> Console.WriteLine("LOG : " + preparedStatement.FormattedText))
  member this.SilentLogger = Action<PreparedStatement>(fun _ -> () )
  member this.Invariant = invariant
  member this.DbProviderFactory = dbProviderFactory
  abstract ConnectionString : string 
  abstract Dialect : IDialect
  abstract SqlParser : Func<string, SqlAst.Statement>
  default this.SqlParser = this.CacheSqlParser
  abstract ExpressionParser : Func<string, ExpressionAst.Expression>
  default this.ExpressionParser = this.CacheExpressionParser
  abstract Logger : Action<PreparedStatement>
  default this.Logger = this.ConsoleLogger
  abstract ConnectionObserver : IConnectionObserver
  default this.CommandObserver = commandObserver
  abstract CommandObserver : ICommandObserver
  default this.ConnectionObserver = connectionObserver
  interface IDbConfig with
    member this.Invariant = this.Invariant
    member this.DbProviderFactory = this.DbProviderFactory
    member this.ConnectionString = this.ConnectionString
    member this.Dialect = this.Dialect
    member this.SqlParser = this.SqlParser
    member this.ExpressionParser = this.ExpressionParser
    member this.Logger = this.Logger
    member this.ConnectionObserver = this.ConnectionObserver
    member this.CommandObserver = this.CommandObserver

[<AbstractClass>]
type MsSqlConfig() = 
  inherit DbConfigBase("System.Data.SqlClient")
  static let dialect = MsSqlDialect() :> IDialect
  override this.Dialect = dialect

[<AbstractClass>]
type MsSqlCeConfig() = 
  inherit DbConfigBase("System.Data.SqlServerCe.4.0")
  static let dialect = MsSqlCeDialect() :> IDialect
  override this.Dialect = dialect

[<AbstractClass>]
type MySqlConfig() = 
  inherit DbConfigBase("MySql.Data.MySqlClient")
  static let dialect = MySqlDialect() :> IDialect
  override this.Dialect = dialect

[<AbstractClass>]
type OracleConfig() = 
  inherit DbConfigBase("Oracle.DataAccess.Client")
  static let dialect = OracleDialect() :> IDialect
  override this.Dialect = dialect

[<AbstractClass>]
type SQLiteConfig() = 
  inherit DbConfigBase("System.Data.SQLite")
  static let dialect = SQLiteDialect() :> IDialect
  override this.Dialect = dialect

type PlainConfig(invariant:string, connectionString:string, dialect:IDialect) =
  inherit DbConfigBase(invariant)
  let mutable logger:Action<PreparedStatement> = base.SilentLogger
  override this.ConnectionString = connectionString
  override this.Dialect = dialect
  override this.Logger = logger
  member this.SetLogger(value) = logger <- value

type IDynamicObject =
  inherit IDictionary
  inherit IDictionary<string, obj>
  inherit ICustomTypeDescriptor

  abstract Dialect : IDialect

  abstract GetCaseSensitiveDict : unit -> IDictionary<string, obj>

type dynamic = IDynamicObject

type DynamicObjectPropertyDescriptor(name:string) =
  inherit PropertyDescriptor(name, null)

  override this.ComponentType = typeof<IDynamicObject>

  override this.IsReadOnly = false

  override this.PropertyType = typeof<obj>

  override this.CanResetValue(``component``:obj) = false

  override this.ResetValue(``component``:obj) = ()

  override this.ShouldSerializeValue(``component``:obj) = false

  override this.GetValue(``component``:obj) =
    let dict = ``component`` :?> IDictionary<string, obj>
    dict.[name]

  override this.SetValue(``component``:obj, value:obj) =
    let dict = ``component`` :?> IDictionary<string, obj>
    dict.[name] <- value

type CaseInsensitiveDynamicObject(dialect:IDialect) =
  inherit DynamicObject()

  let mutable propertyDescriptorCollection:PropertyDescriptorCollection = null

  let syncRoot = obj()

  let dynamicMembers = Dictionary<string, obj>(StringComparer.InvariantCultureIgnoreCase) :> IDictionary<string, obj>

  override this.GetDynamicMemberNames() = 
    dynamicMembers.Keys :> seq<string>

  override this.TrySetMember(binder:SetMemberBinder, value) =
    dynamicMembers.[binder.Name] <- value
    true

  override this.TryGetMember(binder:GetMemberBinder, [<Out>] value:obj byref) =
    dynamicMembers.TryGetValue(binder.Name, &value)

  override this.TryInvokeMember(binder:InvokeMemberBinder, args:obj[], [<Out>] result:obj byref) =
    match dynamicMembers.TryGetValue(binder.Name) with
    | true, ``member`` -> 
      match ``member`` with
      | :? Delegate as ``delegate`` ->
        result <- ``delegate``.DynamicInvoke args
        true
      | _ -> false
    | _ -> false

  member this.SyncRoot = syncRoot

  member this.GetCaseSensitiveDict() = 
    Dictionary<string, obj>(dynamicMembers) :> IDictionary<string, obj>

  member this.Dialect = dialect

  member this.Keys = dynamicMembers.Keys

  member this.Values = dynamicMembers.Values

  member this.Count = dynamicMembers.Count
  
  member this.IsReadOnly = dynamicMembers.IsReadOnly
    
  member this.Item 
      with get (key:string) = 
        dynamicMembers.[key]
      and  set (key:string) value = 
        dynamicMembers.[key] <- value
    
  member this.ContainsKey(key:string) = 
    dynamicMembers.ContainsKey(key)
    
  member this.Contains(item:KeyValuePair<string, obj>) = 
    dynamicMembers.Contains(item)
    
  member this.Add(key:string, value:obj) = 
    dynamicMembers.Add(key, value)
    
  member this.Add(item:KeyValuePair<string, obj>) = 
    dynamicMembers.Add(item)
    
  member this.TryGetValue(key:string, [<Out>] value:obj byref) = 
    dynamicMembers.TryGetValue(key, &value)

  member this.CopyTo(array:KeyValuePair<string, obj>[], arrayIndex) = 
    dynamicMembers.CopyTo(array, arrayIndex)
    
  member this.Remove(key:string) = 
    dynamicMembers.Remove(key)

  member this.Remove(item:KeyValuePair<string, obj>) = 
    dynamicMembers.Remove(item)
    
  member this.Clear() = 
    dynamicMembers.Clear()

  member this.GetEnumerator() = 
    dynamicMembers.GetEnumerator()

  member this.GetAttributes() = AttributeCollection(null)

  member this.GetClassName() = null

  member this.GetComponentName() = null

  member this.GetConverter() = null

  member this.GetDefaultEvent() = null

  member this.GetDefaultProperty() = null

  member this.GetEditor(editorBaseType:Type) = null

  member this.GetEvents() = EventDescriptorCollection(null)

  member this.GetEvents(attributes:Attribute[]) = EventDescriptorCollection(null)

  member this.GetProperties() = this.GetProperties(null)

  member this.GetProperties(attributes:Attribute[]) = 
    if propertyDescriptorCollection = null then
      propertyDescriptorCollection <-
        dynamicMembers.Keys
        |> Seq.map (fun key -> DynamicObjectPropertyDescriptor(key) :> PropertyDescriptor)
        |> Seq.toArray
        |> fun props -> PropertyDescriptorCollection(props)
    propertyDescriptorCollection

  member this.GetPropertyOwner(pd:PropertyDescriptor) = box this

  interface IDynamicObject with
    member this.Dialect = this.Dialect
    member this.GetCaseSensitiveDict() = this.GetCaseSensitiveDict()

  interface IDictionary<string, obj> with
    member this.Keys = this.Keys
    member this.Values = this.Values
    member this.Count = this.Count
    member this.IsReadOnly = this.IsReadOnly
    member this.Item
      with get key = this.[key]
      and  set key value = this.[key] <- value
    member this.ContainsKey(key) = this.ContainsKey(key)
    member this.Contains(item) = this.Contains(item)
    member this.Add(key, value) = this.Add(key, value)
    member this.Add(item:KeyValuePair<string, obj>) = this.Add(item)
    member this.TryGetValue(key, [<Out>] value:obj byref) = this.TryGetValue(key, &value)
    member this.CopyTo(array, arrayIndex) = this.CopyTo(array, arrayIndex)
    member this.Remove(key:string) =  this.Remove(key)
    member this.Remove(item:KeyValuePair<string, obj>) = this.Remove(item)
    member this.Clear() = this.Clear()
    member this.GetEnumerator() = this.GetEnumerator()

  interface IDictionary with
    member this.Keys =  ResizeArray(this.Keys) :> ICollection
    member this.Values = ResizeArray(this.Values) :> ICollection
    member this.Count = this.Count
    member this.IsFixedSize = false
    member this.IsReadOnly = this.IsReadOnly
    member this.IsSynchronized = false
    member this.SyncRoot = this.SyncRoot
    member this.Item
      with get key = this.[string key]
      and  set key value = this.[string key] <- value
    member this.Contains(item) = 
      match item with
      | :? KeyValuePair<string, obj> as pair -> this.Contains(pair)
      | _ -> false
    member this.Add(key, value) = this.Add(string key, value)
    member this.CopyTo(array, arrayIndex) =
      let results = Array.zeroCreate<KeyValuePair<string, obj>> array.Length
      this.CopyTo(results, arrayIndex)
      results |> Array.iteri (fun i item -> array.SetValue(item, i))
    member this.Remove(item) = this.Remove(string item) |> ignore
    member this.Clear() = this.Clear()
    member this.GetEnumerator() = 
      let enumerator = this.GetEnumerator()
      { new IDictionaryEnumerator with 
        member this.MoveNext() = enumerator.MoveNext()
        member this.Current = 
          let pair = enumerator.Current
          upcast DictionaryEntry(box pair.Key, box pair.Value)
        member this.Reset() = enumerator.Reset()
        member this.Entry = 
          let pair = enumerator.Current
          DictionaryEntry(box pair.Key, box pair.Value)
        member this.Key = box enumerator.Current.Key
        member this.Value = box enumerator.Current.Value }

  interface IEnumerable with
    member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator

  interface ICustomTypeDescriptor with
    member this.GetAttributes() = this.GetAttributes()
    member this.GetClassName() = this.GetClassName()
    member this.GetComponentName() = this.GetComponentName()
    member this.GetConverter() = this.GetConverter()
    member this.GetDefaultEvent() = this.GetDefaultEvent()
    member this.GetDefaultProperty() = this.GetDefaultProperty()
    member this.GetEditor(editorBaseType:Type) = this.GetEditor(editorBaseType)
    member this.GetEvents() = this.GetEvents()
    member this.GetEvents(attributes:Attribute[]) = this.GetEvents(attributes)
    member this.GetProperties() = this.GetProperties()
    member this.GetProperties(attributes:Attribute[]) = this.GetProperties(attributes)
    member this.GetPropertyOwner(pd:PropertyDescriptor) = this.GetPropertyOwner(pd)

type DbException (message:Message, ?innerException:exn) =
  inherit InvalidOperationException (message.Format (), match innerException with Some ex -> ex | _ -> null)
  member this.MessageId = message.Id

type DbImpl(config:IDbConfig) =
  
  let dialect = config.Dialect 

  member tihs.NotifyConnectionOpen connection executer =
    let userState = ref null
    config.ConnectionObserver.NotifyOpening(connection, userState)
    executer connection
    config.ConnectionObserver.NotifyOpened(connection, !userState)

  member tihs.NotifyCommandExecute command ps executer =
    let userState = ref null
    config.CommandObserver.NotifyExecuting(command, ps, userState)
    let result = executer command
    config.CommandObserver.NotifyExecuted(command, ps, !userState)
    result

  member this.SetupConnection (connection:DbConnection) =
    connection.ConnectionString <- config.ConnectionString

  member this.SetupCommand (ps:PreparedStatement) (command:DbCommand) =
    command.CommandText <- ps.Text
    ps.Parameters
    |> List.iter (fun param ->
      let dbParam = command.CreateParameter()
      dialect.SetupDbParameter(param, dbParam)
      command.Parameters.Add dbParam |> ignore )

  member this.HandleCommand (ps:PreparedStatement) command commandHandler =
    try
      commandHandler command
    with
    | ex -> 
      if dialect.IsUniqueConstraintViolation(ex) then
        raise <| UniqueConstraintException (ps, ex.Message)
      else 
        reraise ()

  abstract ExecuteCommandOnDemand : PreparedStatement -> (DbCommand -> #seq<'a>) -> seq<'a>
  default this.ExecuteCommandOnDemand (ps:PreparedStatement) commandHandler = 
    seq {
      use connection = config.DbProviderFactory.CreateConnection()
      this.SetupConnection connection
      use command = connection.CreateCommand()
      this.SetupCommand ps command
      config.Logger.Invoke ps
      this.NotifyConnectionOpen connection (fun connection -> connection.Open())
      yield! this.HandleCommand ps command commandHandler }

  member this.ExecuteCommand<'T> ps (commandHandler:DbCommand -> 'T) = 
    let singleton = this.ExecuteCommandOnDemand ps (fun command -> Seq.singleton (commandHandler command))
    Seq.head singleton

  member this.ExecuteReaderOnDemand ps readerHandler = 
    this.ExecuteCommandOnDemand ps (fun command -> seq { 
      use reader = this.HandleCommand ps command (fun c -> this.NotifyCommandExecute command ps (fun command -> command.ExecuteReader()) )
      if not dialect.IsHasRowsPropertySupported || reader.HasRows then
        yield! readerHandler reader
      else
        yield! Seq.empty })

  member this.ExecuteReaderAndScalar readerPs readerHandler scalarPs = 
    this.ExecuteCommand readerPs (fun command -> 
      let results =
        use reader = this.HandleCommand readerPs command (fun c -> this.NotifyCommandExecute command readerPs (fun command -> command.ExecuteReader()) )
        if not dialect.IsHasRowsPropertySupported || reader.HasRows then
          List.ofSeq (readerHandler reader)
        else
          []
      use command = command.Connection.CreateCommand()
      this.SetupCommand scalarPs command
      config.Logger.Invoke scalarPs
      let scalarResult = this.HandleCommand scalarPs command (fun command -> this.NotifyCommandExecute command scalarPs (fun command -> command.ExecuteScalar()))
      results, scalarResult )

  member this.ExecuteReaderWitUserHandler ps handler =
    this.ExecuteCommand ps (fun command -> 
      use reader = this.HandleCommand ps command (fun command -> this.NotifyCommandExecute command ps (fun command -> command.ExecuteReader()) )
      handler reader )

  member this.ExecuteNonQuery ps =
    this.ExecuteCommand ps (fun command -> this.NotifyCommandExecute command ps (fun command -> command.ExecuteNonQuery()) )

  member this.ExecuteScalar ps =
    this.ExecuteCommand ps (fun command -> this.NotifyCommandExecute command ps (fun command -> command.ExecuteScalar()) )

  member this.CreateColumnIndexes (reader:DbDataReader) =
    let length = reader.FieldCount
    let columnIndexes = Dictionary<string, ResizeArray<int>>(length, StringComparer.InvariantCultureIgnoreCase) :> IDictionary<string, ResizeArray<int>>
    for i in 0 .. length - 1 do
      let name = reader.GetName(i)
      match columnIndexes.TryGetValue(name) with
      | true, indexes ->
        indexes.Add(i) |> ignore;
      | _ ->
        let indexes = new ResizeArray<int>()
        indexes.Add(i) |> ignore
        columnIndexes.[name] <- indexes
    let result = Dictionary<string, int>(length, StringComparer.InvariantCultureIgnoreCase) :> IDictionary<string, int>
    columnIndexes |> Seq.iter (fun (KeyValue(name, indexes)) -> 
      indexes |> Seq.iteri (fun i columnIndex -> 
        let uniqueName = if i = 0 then name else name + string i
        result.[uniqueName] <- columnIndex))
    result

  member this.CreatePropMappings (entityMeta:EntityMeta) (columnIndexes:IDictionary<string, int>) =
    let propMappings = Array.zeroCreate entityMeta.PropMetaList.Length
    entityMeta.PropMetaList
    |> List.iteri (fun i propMeta -> 
      propMappings.[i] <-
        match columnIndexes.TryGetValue propMeta.ColumnName with
        | true, columnIndex -> propMeta, Some columnIndex
        | _ -> propMeta, None )
    propMappings

  member this.ConvertFromDbToClr dbValue destType udtTypeName exnHandler =
    try
      dialect.ConvertFromDbToClr(dbValue, destType, udtTypeName)
    with
    | exn ->
      exnHandler exn

  member this.ConvertFromColumnToProp (propMeta:PropMeta) (dbValue:obj) =
    this.ConvertFromDbToClr dbValue propMeta.Type null (fun exn ->
        let typ = if dbValue = null then typeof<obj> else dbValue.GetType()
        raise <| DbException(SR.SOMA4017(typ.FullName, propMeta.ColumnName, propMeta.Type.FullName, propMeta.PropName), exn) )

  member this.MakeDynamicObjectList (reader:DbDataReader) = 
    let fieldCount = reader.FieldCount
    let columnIndexes = this.CreateColumnIndexes reader
    seq { 
      while reader.Read() do
        let dynamic = CaseInsensitiveDynamicObject(dialect)
        columnIndexes
        |> Seq.iter (fun (KeyValue(name, index)) -> 
           let value = reader.GetValue index
           dynamic.[name] <- if value = Convert.DBNull then null else value) 
        yield (box dynamic)  }

  member this.MakeEntity (entityMeta:EntityMeta) (propMappings:(PropMeta * int option) array) (reader:DbDataReader) = 
    let propArray = Array.zeroCreate propMappings.Length
    propMappings 
    |> Array.iter (fun (propMeta, columnIndex) -> 
       let dbValue =
         match columnIndex with
         | Some columnIndex -> reader.GetValue(columnIndex)
         | _ -> Convert.DBNull
       propArray.[propMeta.Index] <- this.ConvertFromColumnToProp propMeta dbValue)
    entityMeta.MakeEntity propArray

  member this.MakeEntityList entityMeta reader = 
    let columnIndexes = this.CreateColumnIndexes reader
    let propMappings = this.CreatePropMappings entityMeta columnIndexes
    seq { 
      while reader.Read() do
        yield this.MakeEntity entityMeta propMappings reader }

  member this.MakeTupleList (tupleMeta:TupleMeta) (reader:DbDataReader) = 
    let fieldCount = reader.FieldCount
    if fieldCount < tupleMeta.BasicElementMetaList.Length then 
      raise <| DbException(SR.SOMA4010())
    let columnIndexes = this.CreateColumnIndexes reader
    let entityMappings =
      tupleMeta.EntityElementMetaList 
      |> List.map (fun elMeta ->
        let propMappings = this.CreatePropMappings elMeta.EntityMeta columnIndexes
        elMeta, propMappings)
    let convertFromColumnToElement (elMeta:BasicElementMeta) (dbValue:obj) =
      this.ConvertFromDbToClr dbValue elMeta.Type null (fun exn ->
        let typ = if dbValue = null then typeof<obj> else dbValue.GetType()
        raise <| DbException(SR.SOMA4018(typ.FullName, elMeta.Index, elMeta.Type.FullName, elMeta.Index), exn) )
    seq { 
      while reader.Read() do
        let tupleAry = Array.zeroCreate (tupleMeta.BasicElementMetaList.Length + tupleMeta.EntityElementMetaList.Length)
        tupleMeta.BasicElementMetaList
        |> Seq.map (fun elMeta -> elMeta, reader.GetValue(elMeta.Index))
        |> Seq.map (fun (elMeta, dbValue) -> elMeta, convertFromColumnToElement elMeta dbValue)
        |> Seq.iter (fun (elMeta, value) -> tupleAry.[elMeta.Index] <- value)
        for elMeta, propMappings in entityMappings do
          tupleAry.[elMeta.Index] <- this.MakeEntity elMeta.EntityMeta propMappings reader
        yield tupleMeta.MakeTuple(tupleAry) }

  member this.MakeSingleList typ (reader:DbDataReader) = 
    let convertFromColumnToReturn (dbValue:obj) =
      this.ConvertFromDbToClr dbValue typ null (fun exn ->
        let typ = if dbValue = null then typeof<obj> else dbValue.GetType()
        raise <| DbException(SR.SOMA4019(typ.FullName, typ.FullName), exn) )
    seq { 
      while reader.Read() do
        let dbValue = reader.GetValue(0)
        yield convertFromColumnToReturn dbValue }

  member this.GetReaderHandler typ =
    if typ = typeof<obj> || typeof<dynamic>.IsAssignableFrom typ then
      (fun reader -> this.MakeDynamicObjectList reader)
    elif Meta.isEntityType typ then 
      (fun reader -> this.MakeEntityList (Meta.makeEntityMeta typ dialect) reader)
    elif FSharpType.IsTuple(typ) then 
      (fun reader -> this.MakeTupleList (Meta.makeTupleMeta typ dialect) reader)
    else 
      (fun reader -> this.MakeSingleList typ reader)

  member this.QueryOnDemand<'T> sql exprCtxt = 
    let typ = typeof<'T>
    let readerHandler = this.GetReaderHandler typ
    let ps = Sql.prepare config sql exprCtxt config.SqlParser
    this.ExecuteReaderOnDemand ps readerHandler
    |> Seq.cast<'T> 

  member this.PaginateOnDemand<'T> sql exprCtxt (offset, limit) : 'T seq = 
    let typ = typeof<'T>
    let readerHandler = this.GetReaderHandler typ
    let ps = Sql.preparePaginate config sql exprCtxt offset limit config.SqlParser
    this.ExecuteReaderOnDemand ps readerHandler
    |> Seq.cast<'T>

  member this.PaginateAndCount<'T>  sql exprCtxt (offset, limit) : 'T list * int64 = 
    let typ = typeof<'T>
    let readerHandler = this.GetReaderHandler typ
    let pagenagePs, countPs = Sql.preparePaginateAndCount config sql exprCtxt offset limit config.SqlParser
    let results, count = this.ExecuteReaderAndScalar pagenagePs readerHandler countPs
    results |> Seq.cast<'T> |> Seq.toList, Convert.ChangeType(count, typeof<int64>) :?> int64

  member this.ExecuteReader<'T> (handler:DbDataReader -> 'T) sql exprCtxt = 
    let ps = Sql.prepare config sql exprCtxt config.SqlParser
    this.ExecuteReaderWitUserHandler ps handler

  member this.FindCore<'T, 'TResult> (idList:obj list) (resultHandler:'T option -> PropMeta option -> PreparedStatement -> 'TResult) = 
    if idList.IsEmpty then
      raise <| DbException(SR.SOMA4004 ())
    let readerHandler, entityMeta = 
      let typ = typeof<'T>
      if Meta.isEntityType typ then
        let entityMeta = Meta.makeEntityMeta typ dialect
        if entityMeta.IdPropMetaList.IsEmpty then
          raise <| DbException(SR.SOMA4005 (typ.FullName))
        elif entityMeta.IdPropMetaList.Length <> idList.Length then
          raise <| DbException(SR.SOMA4003 (entityMeta.IdPropMetaList.Length, idList.Length))
        this.MakeEntityList entityMeta, entityMeta
      else 
        raise <| DbException(SR.SOMA4002 ())
    let ps = Sql.prepareFind config idList entityMeta
    let results = this.ExecuteReaderOnDemand ps readerHandler
    use enumerator = results.GetEnumerator()
    if enumerator.MoveNext() then
      let entity = enumerator.Current
      if enumerator.MoveNext() then
        raise <| DbException(SR.SOMA4016 (ps.Text, ps.Parameters)) 
      else
        resultHandler (Some (entity :?> 'T)) entityMeta.VersionPropMeta ps
    else
      resultHandler None entityMeta.VersionPropMeta ps

  member this.Find<'T when 'T : not struct> idList : 'T = 
    this.FindCore<'T, 'T> idList (fun result _ ps -> 
      match result with
      | Some entity -> 
        entity
      | _ -> 
        raise <| EntityNotFoundException ps )

  member this.TryFind<'T when 'T : not struct> idList : 'T option = 
    this.FindCore<'T, 'T option> idList (fun result _ _ -> result)

  member this.ValidateOptimisticLock version entity (versionPropMeta:PropMeta option) ps =
    match versionPropMeta with
    | Some versionPropMeta ->
      let actualVersion = versionPropMeta.GetValue (upcast entity)
      if actualVersion = null || not <| actualVersion.Equals(version) then
        raise <| OptimisticLockException ps
    | _ -> 
      raise <| OptimisticLockException ps

  member this.FindWithVersion<'T when 'T : not struct> idList (version:obj) : 'T = 
    this.FindCore<'T, 'T> idList (fun result versionPropMeta ps ->
      match result with
      | Some entity -> 
        this.ValidateOptimisticLock version entity versionPropMeta ps
        entity
      | _ -> 
        raise <| EntityNotFoundException ps )

  member this.TryFindWithVersion<'T when 'T : not struct> idList (version:obj) : 'T option = 
    this.FindCore<'T, 'T option> idList (fun result versionPropMeta ps ->
      match result with
      | Some entity -> 
        this.ValidateOptimisticLock version entity versionPropMeta ps
        Some entity
      | _ -> 
        None )

  member this.Execute sql exprCtxt : int = 
    let ps = Sql.prepare config sql exprCtxt config.SqlParser
    this.ExecuteNonQuery ps

  member this.RemakeEntity<'T> (entity:'T, entityMeta:EntityMeta) propHandler =
    let values = 
      entityMeta.PropMetaList
      |> Seq.map (fun propMeta -> propMeta, propMeta.GetValue (upcast entity))
      |> Seq.map propHandler
      |> Seq.toArray
    entityMeta.RemakeEntity (box entity) values :?> 'T

  member this.AppendPreparedStatements ps1 ps2 =
    let text = ps1.Text + "; " + ps2.Text
    let formattedText = ps1.FormattedText + "; " + ps2.FormattedText
    let parameters = 
      List.append (ps1.Parameters) (ps2.Parameters)
    { Text = text; FormattedText = formattedText; Parameters = parameters } 

  member this.ExecuteAndGetFirst ps readerHandler =
    let results = 
      this.ExecuteReaderOnDemand ps (fun reader -> Seq.truncate 1 (readerHandler reader))
      |> Seq.toList
    if results.IsEmpty then
      raise <| NoAffectedRowException ps
    else
      results.Head

  member this.PrepareVersionSelect entity (entityMeta:EntityMeta) (versionPropMeta:PropMeta) =
    let idMetaList = 
      entityMeta.IdPropMetaList
      |> List.map (fun propMeta -> 
        propMeta.ColumnName, propMeta.GetValue(entity), propMeta.Type)
    dialect.PrepareVersionSelect(entityMeta.TableName, versionPropMeta.ColumnName, idMetaList)

  member this.ExecuteAndGetVersionAtOnce entity (entityMeta:EntityMeta) (versionPropMeta:PropMeta) ps =
    let versionPs = this.PrepareVersionSelect entity entityMeta versionPropMeta
    let ps = this.AppendPreparedStatements ps versionPs
    let readerHandler (reader:DbDataReader) =
      seq { while reader.Read() do yield reader.GetValue 0 }
    this.ExecuteAndGetFirst ps readerHandler

  member this.GetVersionOnly entity (entityMeta:EntityMeta) (versionPropMeta:PropMeta) =
    let ps = this.PrepareVersionSelect entity entityMeta versionPropMeta
    let readerHandler (reader:DbDataReader) =
      seq { while reader.Read() do yield reader.GetValue 0 }
    this.ExecuteAndGetFirst ps readerHandler

  member this.FailCauseOfTooManyAffectedRows ps rows =
    raise <| DbException(SR.SOMA4012 (rows, ps.Text, ps.Parameters))

  member this.GetEntityMeta typ =
    if not <| Meta.isEntityType typ then
      raise <| DbException(SR.SOMA4007 ())
    Meta.makeEntityMeta typ dialect

  member this.ConvertFromColumnToPropIfNecessary (dbValueMap:Map<int, obj>) (propMeta:PropMeta, value) =
    match dbValueMap.TryFind propMeta.Index with
    | Some dbValue -> Changed (this.ConvertFromColumnToProp propMeta dbValue)
    | _ -> Unchanged value
  
  member this.PreInsert<'T> (entity:'T) (entityMeta:EntityMeta) =
    let (|Sequence|_|) = function
      | GetSequenceAndInitVersion(idPropMeta, sequenceMeta, _)
      | GetSequence(idPropMeta, sequenceMeta) -> 
        let ps = dialect.PrepareSequenceSelect (sequenceMeta.SqlSequenceName)
        let dbValue = sequenceMeta.Generate config.ConnectionString (fun () -> this.ExecuteScalar ps)
        let value = this.ConvertFromColumnToProp idPropMeta dbValue
        Some (value, idPropMeta)
      | _ -> None
    let (|Version|_|) = function
      | GetSequenceAndInitVersion(_, _, versionPropMeta)
      | InitVersion(versionPropMeta) -> 
         let value = versionPropMeta.GetValue (upcast entity)
         let typ = versionPropMeta.Type
         if Reflection.lessThan (value, typ, 1) then Some (Reflection.one typ, versionPropMeta) else None
      | _ -> None
    match entityMeta.PreInsertCase with
    | Some preInsertCase -> 
      match preInsertCase with
      | Sequence(idValue, idPropMeta) & Version(versionValue, versionPropMeta) -> 
        this.RemakeEntity<'T> (entity, entityMeta) (fun (propMeta:PropMeta, value) ->
          if propMeta.Index = idPropMeta.Index then 
            Changed idValue
          elif propMeta.Index = versionPropMeta.Index then
            Changed versionValue
          else
            Unchanged value )
      | Sequence(idValue, idPropMeta) -> 
        this.RemakeEntity<'T> (entity, entityMeta) (fun (propMeta:PropMeta, value) ->
          if propMeta.Index = idPropMeta.Index then 
            Changed idValue
          else
            Unchanged value )
      | Version(versionValue, versionPropMeta) -> 
        this.RemakeEntity<'T> (entity, entityMeta) (fun (propMeta:PropMeta, value) ->
          if propMeta.Index = versionPropMeta.Index then
            Changed versionValue
          else
            Unchanged value )
      | _ -> 
        entity
    | _ -> 
      entity

  member this.Insert<'T when 'T : not struct> (entity:'T, ?opt:InsertOpt) =
    let entityMeta = this.GetEntityMeta typeof<'T>
    let entity = this.PreInsert entity entityMeta
    let opt = defaultArg opt (InsertOpt())
    let ps = Sql.prepareInsert config entity entityMeta opt
    let makeEntity dbValueMap =
      this.RemakeEntity<'T> (entity, entityMeta) (this.ConvertFromColumnToPropIfNecessary dbValueMap)
    let insert () =
      let rows = this.ExecuteNonQuery ps
      if rows < 1 then 
        raise <| NoAffectedRowException ps
      elif 1 < rows then
        this.FailCauseOfTooManyAffectedRows ps rows
    match entityMeta.InsertCase with
    | InsertThenGetIdentityAndVersionAtOnce(idPropMeta, versionPropMeta) -> 
      let identityPs = 
        dialect.PrepareIdentityAndVersionSelect(entityMeta.TableName, idPropMeta.ColumnName, versionPropMeta.ColumnName)
      let ps = this.AppendPreparedStatements ps identityPs
      let readerHandler (reader:DbDataReader) =
        seq { while reader.Read() do yield reader.GetValue 0, reader.GetValue 1 }
      let idValue, versionValue = this.ExecuteAndGetFirst ps readerHandler
      makeEntity <| map [idPropMeta.Index, idValue; versionPropMeta.Index, versionValue]
    | InsertThenGetIentityAtOnce(idPropMeta) ->
      let identityPs = dialect.PrepareIdentitySelect(entityMeta.TableName, idPropMeta.ColumnName)
      let ps = this.AppendPreparedStatements ps identityPs
      let readerHandler (reader:DbDataReader) =
        seq { while reader.Read() do yield reader.GetValue 0 }
      let idValue = this.ExecuteAndGetFirst ps readerHandler
      makeEntity <| map [idPropMeta.Index, idValue]
    | InsertThenGetVersionAtOnce(versionPropMeta) -> 
      let versionValue = this.ExecuteAndGetVersionAtOnce entity entityMeta versionPropMeta ps
      makeEntity <| map [versionPropMeta.Index, versionValue]
    | InsertThenGetIdentityAndVersionLater(idPropMeta, versionPropMeta) -> 
      insert ()
      let ps = dialect.PrepareIdentityAndVersionSelect(entityMeta.TableName, idPropMeta.ColumnName, versionPropMeta.ColumnName)
      let readerHandler (reader:DbDataReader) =
        seq { while reader.Read() do yield reader.GetValue 0, reader.GetValue 1 }
      let idValue, versionValue = id this.ExecuteAndGetFirst ps readerHandler
      makeEntity <| map [idPropMeta.Index, idValue; versionPropMeta.Index, versionValue]
    | InsertThenGetIdentityLater(idPropMeta) ->
      insert ()
      let ps = dialect.PrepareIdentitySelect(entityMeta.TableName, idPropMeta.ColumnName)
      let readerHandler (reader:DbDataReader) =
        seq { while reader.Read() do yield reader.GetValue 0 }
      let idValue = this.ExecuteAndGetFirst ps readerHandler
      makeEntity <| map [idPropMeta.Index, idValue]
    | InsertThenGetVersionLater(versionPropMeta) ->
      insert ()
      let versionValue = this.GetVersionOnly entity entityMeta versionPropMeta
      makeEntity <| map [versionPropMeta.Index, versionValue]
    | InsertOnly ->
      insert ()
      entity

  member this.Update<'T when 'T : not struct> (entity:'T, ?opt:UpdateOpt) =
    let typ = typeof<'T>
    let entityMeta = this.GetEntityMeta typ
    if entityMeta.IdPropMetaList.IsEmpty then
      raise <| DbException(SR.SOMA4005 (typ.FullName))
    let opt = defaultArg opt (UpdateOpt())
    let ps = Sql.prepareUpdate config entity entityMeta opt
    let update () =
      let rows = this.ExecuteNonQuery ps
      if rows < 1 then
        if opt.IgnoreVersion || entityMeta.VersionPropMeta.IsNone then
          raise <| NoAffectedRowException ps
        else
          raise <| OptimisticLockException ps
      if 1 < rows then 
        this.FailCauseOfTooManyAffectedRows ps rows
    match entityMeta.UpdateCase with
    | UpdateThenGetVersionAtOnce versionPropMeta ->
      let versionValue = this.ExecuteAndGetVersionAtOnce entity entityMeta versionPropMeta ps
      let dbValueMap = map [versionPropMeta.Index, versionValue]
      this.RemakeEntity<'T> (entity, entityMeta) (this.ConvertFromColumnToPropIfNecessary dbValueMap)
    | UpdateThenGetVersionLater versionPropMeta -> 
      update ()
      let versionValue = this.GetVersionOnly entity entityMeta versionPropMeta
      let dbValueMap = map [versionPropMeta.Index, versionValue]
      this.RemakeEntity<'T> (entity, entityMeta) (this.ConvertFromColumnToPropIfNecessary dbValueMap)
    | UpdateThenIncrementVersion versionPropMeta -> 
      update ()
      this.RemakeEntity<'T> (entity, entityMeta) (fun (propMeta:PropMeta, value) ->
        if propMeta.Index = versionPropMeta.Index then
          let typ = propMeta.Type
          Changed (Reflection.incr (value, typ))
        else
          Unchanged value)
    | UpdateOnly ->
      update ()
      entity

  member this.Delete<'T when 'T : not struct> (entity:'T, ?opt:DeleteOpt) =
    let typ = typeof<'T>
    let entityMeta = this.GetEntityMeta typ
    if entityMeta.IdPropMetaList.IsEmpty then
      raise <| DbException(SR.SOMA4005 (typ.FullName))
    let opt = defaultArg opt (DeleteOpt())
    let ps = Sql.prepareDelete config entity entityMeta opt
    let rows = this.ExecuteNonQuery ps
    if rows < 1 then 
      if opt.IgnoreVersion || entityMeta.VersionPropMeta.IsNone then
        raise <| NoAffectedRowException ps
      else 
        raise <| OptimisticLockException ps
    if 1 < rows then 
      this.FailCauseOfTooManyAffectedRows ps rows

  member this.Call<'T when 'T : not struct> (procedure:'T) =
    let typ = typeof<'T>
    let procedureMeta = 
      if not <| Meta.isProcedureType typ then
        raise <| DbException(SR.SOMA4021 ())
      Meta.makeProcedureMeta typ dialect
    let ps = Sql.prepareCall config procedure procedureMeta
    let convertFromDbToClr dbValue (paramMeta:ProcedureParamMeta) =
      this.ConvertFromDbToClr dbValue paramMeta.Type paramMeta.UdtTypeName (fun exn ->
        let typ = if dbValue = null then typeof<obj> else dbValue.GetType()
        raise <| DbException(SR.SOMA4023(typ.FullName, paramMeta.ParamName, procedureMeta.ProcedureName, paramMeta.Type.FullName), exn) )
    this.ExecuteCommand ps (fun command ->
      command.CommandType <- CommandType.StoredProcedure
      let procedureAry = Array.zeroCreate (procedureMeta.ProcedureParamMetaList.Length)
      using (this.NotifyCommandExecute command ps (fun command -> command.ExecuteReader())) (fun reader ->
        procedureMeta.ProcedureParamMetaList
        |> Seq.fold (fun (hasNextResult, reader) paramMeta -> 
          match paramMeta.ParamMetaCase with
          | Result (elementCase, typeConverter) -> 
            if hasNextResult then
              let resultList =
                match elementCase with
                | EntityType entityMeta -> this.MakeEntityList entityMeta reader
                | TupleType tupleMeta -> this.MakeTupleList tupleMeta reader
              procedureAry.[paramMeta.Index] <- Changed (typeConverter resultList)
            reader.NextResult(), reader
          | _ ->
            hasNextResult, reader) (true, reader) |> ignore)
      procedureMeta.ProcedureParamMetaList
      |> Seq.filter (fun paramMeta -> 
        match paramMeta.ParamMetaCase with
        | Result _ -> false
        | _ -> true )
      |> Seq.iter (fun paramMeta -> 
        let paramName = dialect.CreateParameterName paramMeta.ParamName
        let valueCase =
          if command.Parameters.Contains(paramName) then
            let value = command.Parameters.[paramName].Value
            match paramMeta.ParamMetaCase with
            | Unit  -> failwith "unreachable."
            | Input -> Unchanged value
            | _ -> Changed (convertFromDbToClr value paramMeta)
          else 
            Unchanged null
        procedureAry.[paramMeta.Index] <- valueCase)
      procedureMeta.RemakeProcedure (box procedure) procedureAry :?> 'T )

type LocalDbImpl(config:IDbConfig, connection:DbConnection) =
  inherit DbImpl(config)

  override this.ExecuteCommandOnDemand (ps:PreparedStatement) commandHandler = 
    seq {
      use command = connection.CreateCommand()
      this.SetupCommand ps command
      config.Logger.Invoke ps
      if connection.State = ConnectionState.Closed then
        this.NotifyConnectionOpen connection (fun connection -> connection.Open())
      yield! this.HandleCommand ps command commandHandler }

[<RequireQualifiedAccess>]
module Conversion =

  let toIdList(id:obj) =
    match id with
    | :? string ->
      [id]
    | :? IEnumerable as enumerable -> 
      enumerable 
      |> Seq.cast<obj> 
      |> Seq.toList
    | _ -> 
      [id]

  let fromListToExprCtxt(condition:(string * obj * Type) list) =
    let dict = Dictionary<string, obj * Type>() :> IDictionary<string, obj * Type>
    condition 
    |> List.iter(fun (key, value, typ) -> 
      if dict.ContainsKey(key) then
        dict.Remove(key) |> ignore
      dict.[key] <- (value, typ) )
    dict

  let toExprCtxt(condition:obj) =
    let typ = condition.GetType()
    if Meta.isPocoType typ then
      let meta = Meta.makePocoMeta typ
      meta.ExtractProperties condition
    else if Reflection.isGenericDictionary typ then
      let typ = Reflection.getGenericDictionaryType typ
      let prop = typ.GetProperty("Keys")
      let keys = prop.GetValue(condition, null) :?> IEnumerable |> Seq.cast<obj>
      let prop = typ.GetProperty("Values")
      let values = prop.GetValue(condition, null) :?> IEnumerable |> Seq.cast<obj>
      let dict = Dictionary<string, obj * Type>() :> IDictionary<string, obj * Type>
      (keys, values)
      ||> Seq.iter2 (fun key value -> dict.[string key] <- (value, typeof<obj>))
      dict
    else
      match condition with
      | :? list<string * obj * Type> as list -> 
        fromListToExprCtxt list
      | :? IDictionary as d ->
        let dict = Dictionary<string, obj * Type>(d.Count) :> IDictionary<string, obj * Type>
        d.Keys
        |> Seq.cast<obj>
        |> Seq.iter (fun key -> dict.[string key] <- (d.[key], typeof<obj>))
        dict
      | _ -> 
        raise <| DbException(SR.SOMA4020 ())


type IDb =
  abstract DbConfig : IDbConfig
  abstract Query<'T> : string -> 'T IList
  abstract Query<'T> : string * version:obj -> 'T IList
  abstract QueryOnDemand<'T> : string -> 'T seq
  abstract QueryOnDemand<'T> : string * obj -> 'T seq
  abstract Paginate<'T> : string * int64 * int64 -> 'T IList
  abstract Paginate<'T> : string * int64 * int64 * obj -> 'T IList
  abstract PaginateOnDemand<'T> : string * int64 * int64 -> 'T seq
  abstract PaginateOnDemand<'T> : string * int64 * int64 * obj -> 'T seq
  abstract PaginateAndCount<'T> : string * int64 * int64 -> 'T IList * int64
  abstract PaginateAndCount<'T> : string * int64 * int64 * obj -> 'T IList * int64
  abstract Execute : string -> int
  abstract Execute : string * obj -> int
  abstract ExecuteReader<'T> : Func<DbDataReader, 'T> * string -> 'T
  abstract ExecuteReader<'T> : Func<DbDataReader, 'T> * string * obj -> 'T
  abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : obj -> 'T
  abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : obj -> 'T
  abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : obj * obj -> 'T
  abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>  : obj * obj -> 'T
  abstract Insert<'T when 'T : not struct> : 'T -> unit
  abstract Insert<'T when 'T : not struct> : 'T * opt:InsertOpt -> unit
  abstract Update<'T when 'T : not struct> : 'T -> unit
  abstract Update<'T when 'T : not struct> : 'T * opt:UpdateOpt -> unit
  abstract Delete<'T when 'T : not struct> : 'T -> unit
  abstract Delete<'T when 'T : not struct> : 'T * opt:DeleteOpt -> unit
  abstract Call<'T when 'T : not struct> : 'T -> unit

type Db(config:IDbConfig) =

  do Guard.argNotNull (config, "config")

  let db = new DbImpl(config)
    
  abstract DbConfig : IDbConfig
  default this.DbConfig = config

  abstract Query<'T> : sql:string -> 'T IList
  default this.Query<'T>(sql:string) =
    Guard.argNotNull (sql, "sql")
    let seq = db.QueryOnDemand<'T> sql Map.empty
    ResizeArray(seq) :> IList<'T>

  abstract Query<'T> : sql:string * condition:obj -> 'T IList
  default this.Query<'T>(sql:string, condition:obj) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let seq = db.QueryOnDemand<'T> sql exprCtxt
    ResizeArray(seq) :> IList<'T>

  abstract QueryOnDemand<'T> : sql:string -> 'T seq
  default this.QueryOnDemand<'T>(sql:string) =
    Guard.argNotNull (sql, "sql")
    db.QueryOnDemand<'T> sql Map.empty

  abstract QueryOnDemand<'T> : sql:string * condition:obj -> 'T seq
  default this.QueryOnDemand<'T>(sql:string, condition:obj) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    db.QueryOnDemand<'T> sql exprCtxt

  abstract Paginate<'T> : sql:string * offset:int64 * limit:int64 -> 'T IList
  default this.Paginate<'T>(sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (sql, "sql")
    let seq = db.PaginateOnDemand<'T> sql Map.empty (offset, limit)
    ResizeArray(seq) :> IList<'T>

  abstract Paginate<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList
  default this.Paginate<'T>(sql:string, offset:int64, limit:int64, condition:obj) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let seq = db.PaginateOnDemand<'T> sql exprCtxt (offset, limit)
    ResizeArray(seq) :> IList<'T>

  abstract PaginateOnDemand<'T> : sql:string * offset:int64 * limit:int64 -> 'T seq
  default this.PaginateOnDemand<'T>(sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (sql, "sql")
    db.PaginateOnDemand<'T> sql Map.empty (offset, limit)

  abstract PaginateOnDemand<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T seq
  default this.PaginateOnDemand<'T>(sql:string, offset:int64, limit:int64, condition:obj) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    db.PaginateOnDemand<'T> sql exprCtxt (offset, limit)

  abstract PaginateAndCount<'T> : sql:string * offset:int64 * limit:int64 -> 'T IList * int64
  default this.PaginateAndCount<'T>(sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (sql, "sql")
    let list, count = db.PaginateAndCount<'T> sql Map.empty (offset, limit)
    ResizeArray(list) :> IList<'T>, count

  abstract PaginateAndCount<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList * int64
  default this.PaginateAndCount<'T>(sql:string, offset:int64, limit:int64, condition:obj) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let list, count = db.PaginateAndCount<'T> sql exprCtxt (offset, limit)
    ResizeArray(list) :> IList<'T>, count

  abstract Execute : sql:string -> int
  default this.Execute(sql:string) =
    Guard.argNotNull (sql, "sql")
    db.Execute sql Map.empty

  abstract Execute : sql:string * condition:obj -> int
  default this.Execute(sql:string, condition:obj) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    db.Execute sql exprCtxt

  abstract ExecuteReader<'T> : handler:Func<DbDataReader, 'T> * sql:string -> 'T
  default this.ExecuteReader<'T>(handler:Func<DbDataReader, 'T>, sql:string) =
    Guard.argNotNull (handler, "handler")
    Guard.argNotNull (sql, "sql")
    db.ExecuteReader<'T> (fun reader -> handler.Invoke reader) sql Map.empty

  abstract ExecuteReader<'T> : handler:Func<DbDataReader, 'T> * sql:string * condition:obj -> 'T
  default this.ExecuteReader<'T>(handler:Func<DbDataReader, 'T>, sql:string, condition:obj) =
    Guard.argNotNull (handler, "handler")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    db.ExecuteReader<'T> (fun reader -> handler.Invoke reader) sql exprCtxt

  abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : id:obj -> 'T
  default this.Find<'T when 'T : not struct and 'T : (new : unit -> 'T)>(id:obj) =
    Guard.argNotNull (id, "id")
    db.Find<'T> (Conversion.toIdList id)

  abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : id:obj -> 'T
  default this.TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(id:obj) =
    Guard.argNotNull (id, "id")
    match db.TryFind<'T> (Conversion.toIdList id) with
    | Some found -> found
    | _ -> null

  abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : id:obj * version:obj -> 'T
  default this.FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)>(id:obj, version:obj) =
    Guard.argNotNull (id, "id")
    Guard.argNotNull (version, "version")
    db.FindWithVersion<'T> (Conversion.toIdList id) version

  abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>  : id:obj * version:obj -> 'T
  default this.TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(id:obj, version:obj) =
    Guard.argNotNull (id, "id")
    Guard.argNotNull (version, "version")
    match db.TryFindWithVersion<'T> (Conversion.toIdList id) version with
    | Some entity -> entity
    | _ -> null

  abstract Insert<'T when 'T : not struct> : entity:'T -> unit
  default this.Insert<'T when 'T : not struct>(entity) =
    Guard.argNotNull (entity, "entity")
    db.Insert<'T> entity |> ignore

  abstract Insert<'T when 'T : not struct> : entity:'T * opt:InsertOpt -> unit
  default this.Insert<'T when 'T : not struct>(entity, opt) =
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    db.Insert<'T>(entity, opt) |> ignore

  abstract Update<'T when 'T : not struct> : entity:'T -> unit
  default this.Update<'T when 'T : not struct>(entity) =
    Guard.argNotNull (entity, "entity")
    db.Update<'T> entity |> ignore

  abstract Update<'T when 'T : not struct> : entity:'T * opt:UpdateOpt -> unit
  default this.Update<'T when 'T : not struct>(entity, opt) =
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    db.Update<'T>(entity, opt) |> ignore

  abstract Delete<'T when 'T : not struct> : entity:'T -> unit
  default this.Delete<'T when 'T : not struct>(entity) =
    Guard.argNotNull (entity, "entity")
    db.Delete<'T> entity

  abstract Delete<'T when 'T : not struct> : entity:'T * opt:DeleteOpt -> unit
  default this.Delete<'T when 'T : not struct>(entity, opt) =
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    db.Delete<'T>(entity, opt)

  abstract Call<'T when 'T : not struct> : procedure:'T -> unit
  default this.Call<'T when 'T : not struct>(procedure) =
    Guard.argNotNull (procedure, "procedure")
    db.Call<'T> procedure |> ignore

  interface IDb with
    member this.DbConfig = this.DbConfig
    member this.Query<'T>(sql:string) = this.Query<'T>(sql)
    member this.Query<'T>(sql:string, condition:obj) = this.Query<'T>(sql, condition)
    member this.QueryOnDemand<'T>(sql:string) = this.QueryOnDemand<'T>(sql)
    member this.QueryOnDemand<'T>(sql:string, condition:obj) = this.QueryOnDemand<'T>(sql, condition)
    member this.Paginate<'T>(sql:string, offset:int64, limit:int64) = this.Paginate<'T>(sql, offset, limit)
    member this.Paginate<'T>(sql:string, offset:int64, limit:int64, condition:obj) = this.Paginate<'T>(sql, offset, limit, condition)
    member this.PaginateOnDemand<'T>(sql:string, offset:int64, limit:int64) = this.PaginateOnDemand<'T>(sql, offset, limit)
    member this.PaginateOnDemand<'T>(sql:string, offset:int64, limit:int64, condition:obj) = this.PaginateOnDemand<'T>(sql, offset, limit, condition)
    member this.PaginateAndCount<'T>(sql:string, offset:int64, limit:int64) = this.PaginateAndCount<'T>(sql, offset, limit)
    member this.PaginateAndCount<'T>(sql:string, offset:int64, limit:int64, condition:obj) = this.PaginateAndCount<'T>(sql, offset, limit, condition)
    member this.Execute(sql:string) = this.Execute(sql)
    member this.Execute(sql:string, condition:obj) = this.Execute(sql, condition)
    member this.ExecuteReader<'T>(handler:Func<DbDataReader, 'T>, sql:string) = this.ExecuteReader(handler, sql)
    member this.ExecuteReader<'T>(handler:Func<DbDataReader, 'T>, sql:string, condition:obj) = this.ExecuteReader(handler, sql, condition)
    member this.Find<'T when 'T : not struct and 'T : (new : unit -> 'T)>(id:obj) = this.Find<'T>(id)
    member this.TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(id:obj) = this.TryFind<'T>(id)
    member this.FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)>(id:obj, version:obj) = this.FindWithVersion<'T>(id, version)
    member this.TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(id:obj, version:obj) = this.TryFindWithVersion<'T>(id, version)
    member this.Insert<'T when 'T : not struct>(entity:'T) = this.Insert<'T>(entity)
    member this.Insert<'T when 'T : not struct>(entity:'T, opt:InsertOpt) = this.Insert<'T>(entity, opt)
    member this.Update<'T when 'T : not struct>(entity:'T) = this.Update(entity)
    member this.Update<'T when 'T : not struct>(entity:'T, opt:UpdateOpt) = this.Update(entity, opt)
    member this.Delete<'T when 'T : not struct>(entity:'T) = this.Delete<'T>(entity)
    member this.Delete<'T when 'T : not struct>(entity:'T, opt:DeleteOpt) = this.Delete<'T>(entity, opt)
    member this.Call<'T when 'T : not struct>(procedure) = this.Call<'T>(procedure)

type ILocalDb =
  abstract DbConfig : IDbConfig
  abstract Query<'T> : DbConnection * string -> 'T IList
  abstract Query<'T> : DbConnection * string * version:obj -> 'T IList
  abstract QueryOnDemand<'T> : DbConnection * string -> 'T seq
  abstract QueryOnDemand<'T> : DbConnection * string * obj -> 'T seq
  abstract Paginate<'T> : DbConnection * string * int64 * int64 -> 'T IList
  abstract Paginate<'T> : DbConnection * string * int64 * int64 * obj -> 'T IList
  abstract PaginateOnDemand<'T> : DbConnection * string * int64 * int64 -> 'T seq
  abstract PaginateOnDemand<'T> : DbConnection * string * int64 * int64 * obj -> 'T seq
  abstract PaginateAndCount<'T> : DbConnection * string * int64 * int64 -> 'T IList * int64
  abstract PaginateAndCount<'T> : DbConnection * string * int64 * int64 * obj -> 'T IList * int64
  abstract Execute : DbConnection * string -> int
  abstract Execute : DbConnection * string * obj -> int
  abstract ExecuteReader<'T> : DbConnection * Func<DbDataReader, 'T> * string -> 'T
  abstract ExecuteReader<'T> : DbConnection * Func<DbDataReader, 'T> * string * obj -> 'T
  abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : DbConnection * obj -> 'T
  abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : DbConnection * obj -> 'T
  abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : DbConnection * obj * obj -> 'T
  abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>  : DbConnection * obj * obj -> 'T
  abstract Insert<'T when 'T : not struct> : DbConnection * 'T -> unit
  abstract Insert<'T when 'T : not struct> : DbConnection * 'T * opt:InsertOpt -> unit
  abstract Update<'T when 'T : not struct> : DbConnection * 'T -> unit
  abstract Update<'T when 'T : not struct> : DbConnection * 'T * opt:UpdateOpt -> unit
  abstract Delete<'T when 'T : not struct> : DbConnection * 'T -> unit
  abstract Delete<'T when 'T : not struct> : DbConnection * 'T * opt:DeleteOpt -> unit
  abstract Call<'T when 'T : not struct> : DbConnection * 'T -> unit
  abstract CreateConnection : unit -> DbConnection

type LocalDb(config:IDbConfig) =

  do Guard.argNotNull (config, "config")

  abstract DbConfig : IDbConfig
  default this.DbConfig = config

  abstract Query<'T> : connection:DbConnection * sql:string -> 'T IList
  default this.Query<'T>(connection:DbConnection, sql:string) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    let db = LocalDbImpl(config, connection)
    let seq = db.QueryOnDemand<'T> sql Map.empty
    ResizeArray(seq) :> IList<'T>

  abstract Query<'T> : connection:DbConnection * sql:string * condition:obj -> 'T IList
  default this.Query<'T>(connection:DbConnection, sql:string, condition:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let db = LocalDbImpl(config, connection)
    let seq = db.QueryOnDemand<'T> sql exprCtxt
    ResizeArray(seq) :> IList<'T>

  abstract QueryOnDemand<'T> : connection:DbConnection * sql:string -> 'T seq
  default this.QueryOnDemand<'T>(connection:DbConnection, sql:string) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    let db = LocalDbImpl(config, connection)
    db.QueryOnDemand<'T> sql Map.empty

  abstract QueryOnDemand<'T> : connection:DbConnection * sql:string * condition:obj -> 'T seq
  default this.QueryOnDemand<'T>(connection:DbConnection, sql:string, condition:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let db = LocalDbImpl(config, connection)
    db.QueryOnDemand<'T> sql exprCtxt

  abstract Paginate<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T IList
  default this.Paginate<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    let db = LocalDbImpl(config, connection)
    let seq = db.PaginateOnDemand<'T> sql Map.empty (offset, limit)
    ResizeArray(seq) :> IList<'T>

  abstract Paginate<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList
  default this.Paginate<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64, condition:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let db = LocalDbImpl(config, connection)
    let list = db.PaginateOnDemand<'T> sql exprCtxt (offset, limit)
    ResizeArray(list) :> IList<'T>

  abstract PaginateOnDemand<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T seq
  default this.PaginateOnDemand<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    let db = LocalDbImpl(config, connection)
    db.PaginateOnDemand<'T> sql Map.empty  (offset, limit)

  abstract PaginateOnDemand<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T seq
  default this.PaginateOnDemand<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64, condition:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let db = LocalDbImpl(config, connection)
    db.PaginateOnDemand<'T> sql exprCtxt (offset, limit)

  abstract PaginateAndCount<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T IList * int64
  default this.PaginateAndCount<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    let db = LocalDbImpl(config, connection)
    let list, count = db.PaginateAndCount<'T> sql Map.empty  (offset, limit)
    ResizeArray(list) :> IList<'T>, count

  abstract PaginateAndCount<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList * int64
  default this.PaginateAndCount<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64, condition:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let db = LocalDbImpl(config, connection)
    let list, count = db.PaginateAndCount<'T> sql exprCtxt (offset, limit)
    ResizeArray(list) :> IList<'T>, count

  abstract Execute : connection:DbConnection * sql:string -> int
  default this.Execute(connection:DbConnection, sql:string) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    let db = LocalDbImpl(config, connection)
    db.Execute sql Map.empty 

  abstract Execute : connection:DbConnection * sql:string * condition:obj -> int
  default this.Execute(connection:DbConnection, sql:string, condition:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let db = LocalDbImpl(config, connection)
    db.Execute sql exprCtxt

  abstract ExecuteReader<'T> : connection:DbConnection * handler:Func<DbDataReader, 'T> * sql:string -> 'T
  default this.ExecuteReader<'T>(connection:DbConnection, handler:Func<DbDataReader, 'T>, sql:string) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (handler, "handler")
    Guard.argNotNull (sql, "sql")
    let db = LocalDbImpl(config, connection)
    db.ExecuteReader (fun reader -> handler.Invoke reader) sql Map.empty

  abstract ExecuteReader : connection:DbConnection * handler:Func<DbDataReader, 'T> * sql:string * condition:obj -> 'T
  default this.ExecuteReader(connection:DbConnection, handler:Func<DbDataReader, 'T>, sql:string, condition:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (handler, "handler")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = Conversion.toExprCtxt condition
    let db = LocalDbImpl(config, connection)
    db.ExecuteReader (fun reader -> handler.Invoke reader) sql exprCtxt

  abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : connection:DbConnection * id:obj -> 'T
  default this.Find<'T when 'T : not struct and 'T : (new : unit -> 'T)>(connection:DbConnection, id:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (id, "id")
    let db = LocalDbImpl(config, connection)
    db.Find<'T> (Conversion.toIdList id)

  abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : connection:DbConnection * id:obj -> 'T
  default this.TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(connection:DbConnection, id:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (id, "id")
    let db = LocalDbImpl(config, connection)
    match db.TryFind<'T> (Conversion.toIdList id) with
    | Some found -> found
    | _ -> null

  abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : connection:DbConnection * id:obj * version:obj -> 'T
  default this.FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)>(connection:DbConnection, id:obj, version:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (id, "id")
    Guard.argNotNull (version, "version")
    let db = LocalDbImpl(config, connection)
    db.FindWithVersion<'T> (Conversion.toIdList id) version

  abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>  : connection:DbConnection * id:obj * version:obj -> 'T
  default this.TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(connection:DbConnection, id:obj, version:obj) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (id, "id")
    Guard.argNotNull (version, "version")
    let db = LocalDbImpl(config, connection)
    match db.TryFindWithVersion<'T> (Conversion.toIdList id) version with
    | Some entity -> entity
    | _ -> null

  abstract Insert<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit
  default this.Insert<'T when 'T : not struct>(connection:DbConnection, entity) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (entity, "entity")
    let db = LocalDbImpl(config, connection)
    db.Insert<'T> entity |> ignore

  abstract Insert<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:InsertOpt -> unit
  default this.Insert<'T when 'T : not struct>(connection:DbConnection, entity, opt) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    let db = LocalDbImpl(config, connection)
    db.Insert<'T>(entity, opt) |> ignore

  abstract Update<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit
  default this.Update<'T when 'T : not struct>(connection:DbConnection, entity) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (entity, "entity")
    let db = LocalDbImpl(config, connection)
    db.Update<'T> entity |> ignore

  abstract Update<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:UpdateOpt -> unit
  default this.Update<'T when 'T : not struct>(connection:DbConnection, entity, opt) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    let db = LocalDbImpl(config, connection)
    db.Update<'T>(entity, opt) |> ignore

  abstract Delete<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit
  default this.Delete<'T when 'T : not struct>(connection:DbConnection, entity) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (entity, "entity")
    let db = LocalDbImpl(config, connection)
    db.Delete<'T> entity

  abstract Delete<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:DeleteOpt -> unit
  default this.Delete<'T when 'T : not struct>(connection:DbConnection, entity, opt) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    let db = LocalDbImpl(config, connection)
    db.Delete<'T>(entity, opt)

  abstract Call<'T when 'T : not struct> : connection:DbConnection * procedure:'T -> unit
  default this.Call<'T when 'T : not struct>(connection:DbConnection, procedure) =
    Guard.argNotNull (connection, "connection")
    Guard.argNotNull (procedure, "procedure")
    let db = LocalDbImpl(config, connection)
    db.Call<'T> procedure |> ignore

  abstract CreateConnection : unit -> DbConnection
  default this.CreateConnection() =
    let connection = config.DbProviderFactory.CreateConnection()
    connection.ConnectionString <- config.ConnectionString
    connection

  interface ILocalDb with
    member this.DbConfig = this.DbConfig
    member this.Query<'T>(connection:DbConnection, sql:string) = this.Query<'T>(connection, sql)
    member this.Query<'T>(connection:DbConnection, sql:string, condition:obj) = this.Query<'T>(connection, sql, condition)
    member this.QueryOnDemand<'T>(connection:DbConnection, sql:string) = this.QueryOnDemand<'T>(connection, sql)
    member this.QueryOnDemand<'T>(connection:DbConnection, sql:string, condition:obj) = this.QueryOnDemand<'T>(connection, sql, condition)
    member this.Paginate<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64) = this.Paginate<'T>(connection, sql, offset, limit)
    member this.Paginate<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64, condition:obj) = this.Paginate<'T>(connection, sql, offset, limit, condition)
    member this.PaginateOnDemand<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64) = this.PaginateOnDemand<'T>(connection,sql, offset, limit)
    member this.PaginateOnDemand<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64, condition:obj) = this.PaginateOnDemand<'T>(connection, sql, offset, limit, condition)
    member this.PaginateAndCount<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64) = this.PaginateAndCount<'T>(connection, sql, offset, limit)
    member this.PaginateAndCount<'T>(connection:DbConnection, sql:string, offset:int64, limit:int64, condition:obj) = this.PaginateAndCount<'T>(connection, sql, offset, limit, condition)
    member this.Execute(connection:DbConnection, sql:string) = this.Execute(connection, sql)
    member this.Execute(connection:DbConnection, sql:string, condition:obj) = this.Execute(connection, sql, condition)
    member this.ExecuteReader<'T>(connection:DbConnection, handler:Func<DbDataReader, 'T>, sql:string) = this.ExecuteReader(connection, handler, sql)
    member this.ExecuteReader<'T>(connection:DbConnection, handler:Func<DbDataReader, 'T>, sql:string, condition:obj) = this.ExecuteReader(connection, handler, sql, condition)
    member this.Find<'T when 'T : not struct and 'T : (new : unit -> 'T)>(connection:DbConnection, id:obj) = this.Find<'T>(connection, id)
    member this.TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(connection:DbConnection, id:obj) = this.TryFind<'T>(connection, id)
    member this.FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)>(connection:DbConnection, id:obj, version:obj) = this.FindWithVersion<'T>(connection, id, version)
    member this.TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(connection:DbConnection, id:obj, version:obj) = this.TryFindWithVersion<'T>(connection, id, version)
    member this.Insert<'T when 'T : not struct>(connection:DbConnection, entity:'T) = this.Insert<'T>(connection, entity)
    member this.Insert<'T when 'T : not struct>(connection:DbConnection, entity:'T, opt:InsertOpt) = this.Insert<'T>(connection, entity, opt)
    member this.Update<'T when 'T : not struct>(connection:DbConnection, entity:'T) = this.Update<'T>(connection, entity)
    member this.Update<'T when 'T : not struct>(connection:DbConnection, entity:'T, opt:UpdateOpt) = this.Update<'T>(connection, entity, opt)
    member this.Delete<'T when 'T : not struct>(connection:DbConnection, entity:'T) = this.Delete<'T>(connection, entity)
    member this.Delete<'T when 'T : not struct>(connection:DbConnection, entity:'T, opt:DeleteOpt) = this.Delete<'T>(connection, entity, opt)
    member this.Call<'T when 'T : not struct>(connection:DbConnection, procedure) = this.Call<'T>(connection, procedure)
    member this.CreateConnection() = this.CreateConnection()

type PlainDb(config:IDbConfig) =

  do Guard.argNotNull (config, "config")

  let db = new DbImpl(config)

  let createExprCtxt (condition:IDictionary) =
    let exprCtxt = Dictionary<string, obj * Type>(condition.Count)
    condition.Keys
    |> Seq.cast<obj>
    |> Seq.iter (fun key -> 
      let key = if key <> null then key.ToString() else null
      let value = condition.[key]
      let typ = if value = null || value = Convert.DBNull then typeof<obj> else value.GetType()
      exprCtxt.[key] <- (value, typ))
    exprCtxt

  let convertToHashtable (dict:IDictionary) =
     Hashtable(dict, StringComparer.InvariantCultureIgnoreCase) :> IDictionary

  abstract DbConfig : IDbConfig
  default this.DbConfig = config

  abstract Query : sql:string -> IDictionary IList
  default this.Query(sql:string) =
    Guard.argNotNull (sql, "sql")
    let seq = 
      db.QueryOnDemand<dynamic> sql Map.empty
      |> Seq.map convertToHashtable
    ResizeArray(seq) :> IList<IDictionary>

  abstract Query : sql:string  * condition:IDictionary -> IDictionary IList
  default this.Query(sql:string, condition:IDictionary) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = createExprCtxt condition
    let seq = 
      db.QueryOnDemand<dynamic> sql exprCtxt
      |> Seq.map convertToHashtable
    ResizeArray(seq) :> IList<IDictionary>

  abstract QueryOnDemand : sql:string -> IDictionary seq  
  default this.QueryOnDemand(sql:string) =
    Guard.argNotNull (sql, "sql")
    db.QueryOnDemand<dynamic> sql Map.empty
    |> Seq.map convertToHashtable

  abstract QueryOnDemand : sql:string * condition:IDictionary -> IDictionary seq  
  default this.QueryOnDemand(sql:string, condition:IDictionary) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = createExprCtxt condition
    db.QueryOnDemand<dynamic> sql exprCtxt
    |> Seq.map convertToHashtable

  abstract Paginate : sql:string * offset:int64 * limit:int64 -> IDictionary IList
  default this.Paginate(sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (sql, "sql")
    let seq = 
      db.PaginateOnDemand<dynamic> sql Map.empty (offset, limit)
      |> Seq.map convertToHashtable
    ResizeArray(seq) :> IList<IDictionary>

  abstract Paginate : sql:string * offset:int64 * limit:int64 * condition:IDictionary -> IDictionary IList
  default this.Paginate(sql:string, offset:int64, limit:int64, condition:IDictionary) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = createExprCtxt condition
    let seq = 
      db.PaginateOnDemand<dynamic> sql exprCtxt (offset, limit)
      |> Seq.map convertToHashtable
    ResizeArray(seq) :> IList<IDictionary>

  abstract PaginateOnDemand : sql:string * offset:int64 * limit:int64 -> IDictionary seq
  default this.PaginateOnDemand(sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (sql, "sql")
    db.PaginateOnDemand<dynamic> sql Map.empty (offset, limit)
    |> Seq.map convertToHashtable

  abstract PaginateOnDemand : sql:string * offset:int64 * limit:int64 * condition:IDictionary -> IDictionary seq
  default this.PaginateOnDemand(sql:string, offset:int64, limit:int64, condition:IDictionary) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = createExprCtxt condition
    db.PaginateOnDemand<dynamic> sql exprCtxt (offset, limit)
    |> Seq.map convertToHashtable

  abstract PaginateAndCount : sql:string * offset:int64 * limit:int64 -> IDictionary IList * int64
  default this.PaginateAndCount(sql:string, offset:int64, limit:int64) =
    Guard.argNotNull (sql, "sql")
    let list, count = db.PaginateAndCount<dynamic> sql Map.empty (offset, limit)
    let seq = list |> Seq.map convertToHashtable
    ResizeArray(seq) :> IList<IDictionary>, count

  abstract PaginateAndCount : sql:string * offset:int64 * limit:int64 * condition:IDictionary -> IDictionary IList * int64
  default this.PaginateAndCount(sql:string, offset:int64, limit:int64, condition:IDictionary) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let exprCtxt = createExprCtxt condition
    let list, count = 
      db.PaginateAndCount<dynamic> sql exprCtxt (offset, limit)
    let seq = list |> Seq.map convertToHashtable
    ResizeArray(seq) :> IList<IDictionary>, count

  abstract Execute : sql:string -> int
  default this.Execute(sql:string) =
    Guard.argNotNull (sql, "sql")
    db.Execute sql Map.empty 

  abstract Execute : sql:string * condition:IDictionary -> int
  default this.Execute(sql:string, condition:IDictionary) =
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    db.Execute sql (createExprCtxt condition)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Db =

  let query<'T> config sql condition = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition") 
    let db = DbImpl config
    db.QueryOnDemand<'T> sql (Conversion.fromListToExprCtxt condition)
    |> Seq.toList

  let queryOnDemand<'T> config sql condition = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition") 
    let db = DbImpl config
    db.QueryOnDemand<'T> sql (Conversion.fromListToExprCtxt condition)

  let paginate<'T> config sql condition (offset, limit) = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition") 
    let db = DbImpl config
    db.PaginateOnDemand<'T> sql (Conversion.fromListToExprCtxt condition) (offset, limit)
    |> Seq.toList

  let paginateOnDemand<'T> config sql condition (offset, limit) = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition") 
    let db = DbImpl config
    db.PaginateOnDemand<'T> sql (Conversion.fromListToExprCtxt condition) (offset, limit)

  let paginateAndCount<'T> config sql condition (offset, limit) = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let db = DbImpl config
    db.PaginateAndCount<'T> sql (Conversion.fromListToExprCtxt condition) (offset, limit)

  let find<'T when 'T : not struct> config id = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (id, "id")
    let db = DbImpl config
    db.Find<'T> id

  let tryFind<'T when 'T : not struct> config id = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (id, "id")
    let db = DbImpl config
    db.TryFind<'T> id

  let findWithVersion<'T when 'T : not struct> config id version = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (id, "id")
    let db = DbImpl config
    db.FindWithVersion<'T> id version

  let tryFindWithVersion<'T when 'T : not struct> config id version = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (id, "id")
    let db = DbImpl config
    db.TryFindWithVersion<'T> id version

  let execute config sql condition = 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let db = DbImpl config
    db.Execute sql (Conversion.fromListToExprCtxt condition)

  let executeReader<'T> config handler sql condition =
    Guard.argNotNull (config, "config")
    Guard.argNotNull (handler, "handler")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let db = DbImpl config
    db.ExecuteReader<'T> handler sql (Conversion.fromListToExprCtxt condition)

  let insert<'T when 'T : not struct> config entity =
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    let db = DbImpl config
    db.Insert<'T> entity

  let insertWithOpt<'T when 'T : not struct> config entity opt =
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt") 
    let db = DbImpl config
    db.Insert<'T>(entity, opt)

  let update<'T when 'T : not struct> config entity =
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    let db = DbImpl config
    db.Update<'T> entity

  let updateWithOpt<'T when 'T : not struct> config entity opt =
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt") 
    let db = DbImpl(config)
    db.Update<'T>(entity, opt)

  let delete<'T when 'T : not struct> config entity =
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    let db = DbImpl config
    db.Delete<'T> entity

  let deleteWithOpt<'T when 'T : not struct> config entity opt =
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt") 
    let db = DbImpl config
    db.Delete<'T>(entity, opt)

  let call<'T when 'T : not struct> config procedure =
    Guard.argNotNull (config, "config")
    Guard.argNotNull (procedure, "procedure")
    let db = DbImpl config
    db.Call<'T> procedure

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LocalDb =

  let query<'T> config connection sql condition = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition") 
    let db = LocalDbImpl(config, connection)
    db.QueryOnDemand<'T> sql (Conversion.fromListToExprCtxt condition)
    |> Seq.toList

  let queryOnDemand<'T> config connection sql condition = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition") 
    let db = LocalDbImpl(config, connection)
    db.QueryOnDemand<'T> sql (Conversion.fromListToExprCtxt condition)

  let paginate<'T> config connection sql condition (offset, limit) = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition") 
    let db = LocalDbImpl(config, connection)
    db.PaginateOnDemand<'T> sql (Conversion.fromListToExprCtxt condition) (offset, limit)
    |> Seq.toList

  let paginateOnDemand<'T> config connection sql condition (offset, limit) = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition") 
    let db = LocalDbImpl(config, connection)
    db.PaginateOnDemand<'T> sql (Conversion.fromListToExprCtxt condition) (offset, limit)

  let paginateAndCount<'T> config connection sql condition (offset, limit) = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let db = LocalDbImpl(config, connection)
    db.PaginateAndCount<'T> sql (Conversion.fromListToExprCtxt condition) (offset, limit)

  let find<'T when 'T : not struct> config connection id = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (id, "id")
    let db = LocalDbImpl(config, connection)
    db.Find<'T> id

  let tryFind<'T when 'T : not struct> config connection id = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (id, "id")
    let db = LocalDbImpl(config, connection)
    db.TryFind<'T> id

  let findWithVersion<'T when 'T : not struct> config connection id version = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (id, "id")
    let db = LocalDbImpl(config, connection)
    db.FindWithVersion<'T> id version

  let tryFindWithVersion<'T when 'T : not struct> config connection id version = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (id, "id")
    let db = LocalDbImpl(config, connection)
    db.TryFindWithVersion<'T> id version

  let execute config connection sql condition = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let db = LocalDbImpl(config, connection)
    db.Execute sql (Conversion.fromListToExprCtxt condition)

  let executeReader<'T> config connection handler sql condition = 
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (handler, "handler")
    Guard.argNotNull (sql, "sql")
    Guard.argNotNull (condition, "condition")
    let db = LocalDbImpl(config, connection)
    db.ExecuteReader<'T> handler sql (Conversion.fromListToExprCtxt condition)

  let insert<'T when 'T : not struct> config connection entity =
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    let db = LocalDbImpl(config, connection)
    db.Insert<'T> entity

  let insertWithOpt<'T when 'T : not struct> config connection entity opt =
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    let db = LocalDbImpl(config, connection)
    db.Insert<'T>(entity, opt)

  let update<'T when 'T : not struct> config connection entity =
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    let db = LocalDbImpl(config, connection)
    db.Update<'T> entity
 
  let updateWithOpt<'T when 'T : not struct> config connection entity opt =
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    let db = LocalDbImpl(config, connection)
    db.Update<'T>(entity, opt)

  let delete<'T when 'T : not struct> config connection entity =
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    let db = LocalDbImpl(config, connection)
    db.Delete<'T> entity

  let deleteWithOpt<'T when 'T : not struct> config connection entity opt =
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (entity, "entity")
    Guard.argNotNull (opt, "opt")
    let db = LocalDbImpl(config, connection)
    db.Delete<'T>(entity, opt)

  let call<'T when 'T : not struct> config connection procedure =
    Guard.argNotNull (connection, "connection") 
    Guard.argNotNull (config, "config")
    Guard.argNotNull (procedure, "procedure")
    let db = LocalDbImpl(config, connection)
    db.Call<'T> procedure

  let createConnection (config:IDbConfig) =
    let connection = config.DbProviderFactory.CreateConnection()
    connection.ConnectionString <- config.ConnectionString
    connection

[<AutoOpen>]
module DynamicOperations =

  let (?) (dynamic:dynamic) (propName:string) :'a =
    let value = dynamic.[propName]
    let destType = typeof<'a>
    try
      dynamic.Dialect.ConvertFromDbToClr(value, destType, null) :?> 'a
    with
    | exn ->
      let typ = if value = null then typeof<obj> else value.GetType()
      raise <| DbException(SR.SOMA4027(typ.FullName, propName, destType.FullName), exn)

  let inline (?<-) (dynamic:dynamic) (propName:string) (value:'a) =
    dynamic.[propName] <- value

  [<CompiledName("Dynamic")>]
  let dynamic dialect = 
    Guard.argNotNull(dialect, "dialect")
    CaseInsensitiveDynamicObject(dialect) :> dynamic

[<AutoOpen>]
module UtilityOperations =

  let inline (@=) (name:string) (value:'a) = name, box value, typeof<'a>