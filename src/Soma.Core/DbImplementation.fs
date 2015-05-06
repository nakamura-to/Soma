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
open FSharp.QueryProvider

type internal DbImpl(config : IDbConfig) as this = 
    let dialect = config.Dialect
    
    let queryProvider = lazy (
        Queryable.DBQueryProvider
            ((fun () -> 
             let connection = config.DbProviderFactory.CreateConnection()
             this.SetupConnection connection
             connection), 
             (fun connection expression ->
                let q,ctor = config.QueryTranslator QueryType.SelectQuery connection expression
                let ctor =
                    match ctor with
                    | Some ctor -> ctor
                    | None -> failwith "no ctorinfor generated"
                q, ctor), 
             Some(fun command -> 
                 let userState = ref null
                 config.CommandObserver.NotifyExecuting(command, userState)
                 command, !userState), 
             Some(fun (command, userState) -> config.CommandObserver.NotifyExecuted(command, userState))))
    
    let checkCanDelete entityMeta = 
        if entityMeta.IdPropMetaList.IsEmpty then 
            raise <| Soma.Core.DbException(SR.SOMA4005(entityMeta.Type.FullName))
        
    member this.NotifyConnectionOpen connection executer = 
        let userState = ref null
        config.ConnectionObserver.NotifyOpening(connection, userState)
        executer connection
        config.ConnectionObserver.NotifyOpened(connection, !userState)
    
    member this.NotifyCommandExecute command executer = 
        let userState = ref null
        config.CommandObserver.NotifyExecuting(command, userState)
        let result = executer command
        config.CommandObserver.NotifyExecuted(command, !userState)
        result
    
    member this.SetupConnection(connection : DbConnection) = connection.ConnectionString <- config.ConnectionString
    
    member this.SetupCommand (ps : PreparedStatement) (command : DbCommand) = 
        command.CommandText <- ps.Text
        ps.Parameters |> List.iter (fun param -> 
                             let dbParam = command.CreateParameter()
                             dialect.SetupDbParameter(param, dbParam)
                             command.Parameters.Add dbParam |> ignore)
        config.Dialect.MakeParametersDisposer command
    
    member this.HandleCommand (ps : PreparedStatement) command commandHandler = 
        try 
            commandHandler command
        with ex -> 
            if dialect.IsUniqueConstraintViolation(ex) then raise <| UniqueConstraintException(ps, ex.Message, ex)
            else reraise()
    
    abstract ExecuteCommandOnDemand : PreparedStatement -> (DbCommand -> #seq<'a>) -> seq<'a>
    
    override this.ExecuteCommandOnDemand (ps : PreparedStatement) commandHandler = 
        seq { 
            use connection = config.DbProviderFactory.CreateConnection()
            this.SetupConnection connection
            use command = connection.CreateCommand()
            use paramsDisposer = this.SetupCommand ps command
            config.Logger.Invoke ps
            this.NotifyConnectionOpen connection (fun connection -> connection.Open())
            yield! this.HandleCommand ps command commandHandler
        }
    
    member this.ExecuteCommand<'T> ps (commandHandler : DbCommand -> 'T) = 
        let singleton = this.ExecuteCommandOnDemand ps (fun command -> Seq.singleton (commandHandler command))
        Seq.head singleton
    
    member this.ExecuteReaderOnDemand ps readerHandler = 
        this.ExecuteCommandOnDemand ps (fun command -> 
            seq { 
                use reader = 
                    this.HandleCommand ps command 
                        (fun c -> this.NotifyCommandExecute command (fun command -> command.ExecuteReader()))
                if not dialect.IsHasRowsPropertySupported || reader.HasRows then yield! readerHandler reader
                else yield! Seq.empty
            })
    
    member this.ExecuteReaderAndScalar readerPs readerHandler scalarPs = 
        this.ExecuteCommand readerPs (fun command -> 
            let results = 
                use reader = 
                    this.HandleCommand readerPs command 
                        (fun c -> this.NotifyCommandExecute command (fun command -> command.ExecuteReader()))
                if not dialect.IsHasRowsPropertySupported || reader.HasRows then List.ofSeq (readerHandler reader)
                else []
            
            use command = command.Connection.CreateCommand()
            use paramsDisposer = this.SetupCommand scalarPs command
            config.Logger.Invoke scalarPs
            let scalarResult = 
                this.HandleCommand scalarPs command 
                    (fun command -> this.NotifyCommandExecute command (fun command -> command.ExecuteScalar()))
            results, scalarResult)
    
    member this.ExecuteReaderWitUserHandler ps handler = 
        this.ExecuteCommand ps (fun command -> 
            use reader = 
                this.HandleCommand ps command 
                    (fun command -> this.NotifyCommandExecute command (fun command -> command.ExecuteReader()))
            handler reader)
    
    member this.ExecuteNonQuery ps = 
        this.ExecuteCommand ps 
            (fun command -> this.NotifyCommandExecute command (fun command -> command.ExecuteNonQuery()))
    member this.ExecuteScalar ps = 
        this.ExecuteCommand ps 
            (fun command -> this.NotifyCommandExecute command (fun command -> command.ExecuteScalar()))
    
    member this.CreateColumnIndexes(reader : DbDataReader) = 
        let length = reader.FieldCount
        let columnIndexes = 
            Dictionary<string, ResizeArray<int>>(length, StringComparer.InvariantCultureIgnoreCase) :> IDictionary<string, ResizeArray<int>>
        for i in 0..length - 1 do
            let name = reader.GetName(i)
            match columnIndexes.TryGetValue(name) with
            | true, indexes -> indexes.Add(i) |> ignore
            | _ -> 
                let indexes = new ResizeArray<int>()
                indexes.Add(i) |> ignore
                columnIndexes.[name] <- indexes
        let result = 
            Dictionary<string, int>(length, StringComparer.InvariantCultureIgnoreCase) :> IDictionary<string, int>
        columnIndexes |> Seq.iter (fun (KeyValue(name, indexes)) -> 
                             indexes |> Seq.iteri (fun i columnIndex -> 
                                            let uniqueName = 
                                                if i = 0 then name
                                                else name + string i
                                            result.[uniqueName] <- columnIndex))
        result
    
    member this.CreatePropMappings (entityMeta : EntityMeta) (columnIndexes : IDictionary<string, int>) = 
        let propMappings = Array.zeroCreate entityMeta.PropMetaList.Length
        entityMeta.PropMetaList |> List.iteri (fun i propMeta -> 
                                       propMappings.[i] <- match columnIndexes.TryGetValue propMeta.ColumnName with
                                                           | true, columnIndex -> propMeta, Some columnIndex
                                                           | _ -> propMeta, None)
        propMappings
    
    member this.ConvertFromDbToClr dbValue destType udtTypeName prop exnHandler = 
        try 
            dialect.ConvertFromDbToClr(dbValue, destType, udtTypeName, prop)
        with exn -> exnHandler exn
    
    member this.ConvertFromColumnToProp (propMeta : PropMeta) (dbValue : obj) = 
        this.ConvertFromDbToClr dbValue propMeta.Type null propMeta.Property 
            (fun exn -> 
            let typ = 
                if dbValue = null then typeof<obj>
                else dbValue.GetType()
            raise 
            <| Soma.Core.DbException
                   (SR.SOMA4017(typ.FullName, propMeta.ColumnName, propMeta.Type.FullName, propMeta.PropName), exn))
    
    member this.MakeDynamicObjectList(reader : DbDataReader) = 
        let fieldCount = reader.FieldCount
        let columnIndexes = this.CreateColumnIndexes reader
        seq { 
            while reader.Read() do
                let dynamic = CaseInsensitiveDynamicObject(dialect)
                columnIndexes |> Seq.iter (fun (KeyValue(name, index)) -> 
                                     let value = dialect.GetValue(reader, index, null)
                                     dynamic.[name] <- if value = Convert.DBNull then null
                                                       else value)
                yield (box dynamic)
        }
    
    member this.MakeEntity (entityMeta : EntityMeta) (propMappings : (PropMeta * int option) array) 
           (reader : DbDataReader) = 
        let propArray = Array.zeroCreate propMappings.Length
        propMappings |> Array.iter (fun (propMeta, columnIndex) -> 
                            let dbValue = 
                                match columnIndex with
                                | Some columnIndex -> dialect.GetValue(reader, columnIndex, propMeta.Property)
                                | _ -> Convert.DBNull
                            propArray.[propMeta.Index] <- this.ConvertFromColumnToProp propMeta dbValue)
        entityMeta.MakeEntity propArray
    
    member this.MakeEntityList entityMeta reader = 
        let columnIndexes = this.CreateColumnIndexes reader
        let propMappings = this.CreatePropMappings entityMeta columnIndexes
        seq { 
            while reader.Read() do
                yield this.MakeEntity entityMeta propMappings reader
        }
    
    member this.MakeTupleList (tupleMeta : TupleMeta) (reader : DbDataReader) = 
        let fieldCount = reader.FieldCount
        if fieldCount < tupleMeta.BasicElementMetaList.Length then raise <| Soma.Core.DbException(SR.SOMA4010())
        let columnIndexes = this.CreateColumnIndexes reader
        
        let entityMappings = 
            tupleMeta.EntityElementMetaList |> List.map (fun elMeta -> 
                                                   let propMappings = 
                                                       this.CreatePropMappings elMeta.EntityMeta columnIndexes
                                                   elMeta, propMappings)
        
        let convertFromColumnToElement (elMeta : BasicElementMeta) (dbValue : obj) = 
            this.ConvertFromDbToClr dbValue elMeta.Type null null 
                (fun exn -> 
                let typ = 
                    if dbValue = null then typeof<obj>
                    else dbValue.GetType()
                raise 
                <| Soma.Core.DbException
                       (SR.SOMA4018(typ.FullName, elMeta.Index, elMeta.Type.FullName, elMeta.Index), exn))
        
        seq { 
            while reader.Read() do
                let tupleAry = 
                    Array.zeroCreate (tupleMeta.BasicElementMetaList.Length + tupleMeta.EntityElementMetaList.Length)
                tupleMeta.BasicElementMetaList
                |> Seq.map (fun elMeta -> elMeta, dialect.GetValue(reader, elMeta.Index, null))
                |> Seq.map (fun (elMeta, dbValue) -> elMeta, convertFromColumnToElement elMeta dbValue)
                |> Seq.iter (fun (elMeta, value) -> tupleAry.[elMeta.Index] <- value)
                for elMeta, propMappings in entityMappings do
                    tupleAry.[elMeta.Index] <- this.MakeEntity elMeta.EntityMeta propMappings reader
                yield tupleMeta.MakeTuple(tupleAry)
        }
    
    member this.MakeSingleList typ (reader : DbDataReader) = 
        let convertFromColumnToReturn (dbValue : obj) = 
            this.ConvertFromDbToClr dbValue typ null null (fun exn -> 
                let typ = 
                    if dbValue = null then typeof<obj>
                    else dbValue.GetType()
                raise <| Soma.Core.DbException(SR.SOMA4019(typ.FullName, typ.FullName), exn))
        seq { 
            while reader.Read() do
                let dbValue = dialect.GetValue(reader, 0, null)
                yield convertFromColumnToReturn dbValue
        }
    
    member this.GetReaderHandler typ = 
        if typ = typeof<obj> || typeof<dynamic>.IsAssignableFrom typ then 
            (fun reader -> this.MakeDynamicObjectList reader)
        elif Meta.isEntityType typ then (fun reader -> this.MakeEntityList (Meta.makeEntityMeta typ dialect) reader)
        elif FSharpType.IsTuple(typ) then (fun reader -> this.MakeTupleList (Meta.makeTupleMeta typ dialect) reader)
        else (fun reader -> this.MakeSingleList typ reader)
    
    member this.QueryOnDemand<'T> sql exprCtxt = 
        let typ = typeof<'T>
        let readerHandler = this.GetReaderHandler typ
        let ps = Sql.prepare config sql exprCtxt config.SqlParser
        this.ExecuteReaderOnDemand ps readerHandler |> Seq.cast<'T>
    
    member this.PaginateOnDemand<'T> sql exprCtxt (offset, limit) : 'T seq = 
        let typ = typeof<'T>
        let readerHandler = this.GetReaderHandler typ
        let ps = Sql.preparePaginate config sql exprCtxt offset limit config.SqlParser
        this.ExecuteReaderOnDemand ps readerHandler |> Seq.cast<'T>
    
    member this.PaginateAndCount<'T> sql exprCtxt (offset, limit) : 'T list * int64 = 
        let typ = typeof<'T>
        let readerHandler = this.GetReaderHandler typ
        let pagenagePs, countPs = Sql.preparePaginateAndCount config sql exprCtxt offset limit config.SqlParser
        let results, count = this.ExecuteReaderAndScalar pagenagePs readerHandler countPs
        results
        |> Seq.cast<'T>
        |> Seq.toList, Convert.ChangeType(count, typeof<int64>) :?> int64
    
    member this.ExecuteReader<'T> (handler : DbDataReader -> 'T) sql exprCtxt = 
        let ps = Sql.prepare config sql exprCtxt config.SqlParser
        this.ExecuteReaderWitUserHandler ps handler
    
    member this.FindCore<'T, 'TResult> (idList : obj list) 
           (resultHandler : 'T option -> PropMeta option -> PreparedStatement -> 'TResult) = 
        if idList.IsEmpty then raise <| Soma.Core.DbException(SR.SOMA4004())
        let readerHandler, entityMeta = 
            let typ = typeof<'T>
            if Meta.isEntityType typ then 
                let entityMeta = Meta.makeEntityMeta typ dialect
                if entityMeta.IdPropMetaList.IsEmpty then raise <| Soma.Core.DbException(SR.SOMA4005(typ.FullName))
                else 
                    if entityMeta.IdPropMetaList.Length <> idList.Length then 
                        raise <| Soma.Core.DbException(SR.SOMA4003(entityMeta.IdPropMetaList.Length, idList.Length))
                this.MakeEntityList entityMeta, entityMeta
            else raise <| Soma.Core.DbException(SR.SOMA4002())
        
        let ps = Sql.prepareFind config idList entityMeta
        let results = this.ExecuteReaderOnDemand ps readerHandler
        use enumerator = results.GetEnumerator()
        if enumerator.MoveNext() then 
            let entity = enumerator.Current
            if enumerator.MoveNext() then raise <| Soma.Core.DbException(SR.SOMA4016(ps.Text, ps.Parameters))
            else resultHandler (Some(entity :?> 'T)) entityMeta.VersionPropMeta ps
        else resultHandler None entityMeta.VersionPropMeta ps
    
    member this.Find<'T when 'T : not struct> idList : 'T = 
        this.FindCore<'T, 'T> idList (fun result _ ps -> 
            match result with
            | Some entity -> entity
            | _ -> raise <| EntityNotFoundException ps)
    
    member this.TryFind<'T when 'T : not struct> idList : 'T option = 
        this.FindCore<'T, 'T option> idList (fun result _ _ -> result)
    
    member this.ValidateOptimisticLock version entity (versionPropMeta : PropMeta option) ps = 
        match versionPropMeta with
        | Some versionPropMeta -> 
            let actualVersion = versionPropMeta.GetValue(upcast entity)
            if actualVersion = null || not <| actualVersion.Equals(version) then raise <| OptimisticLockException ps
        | _ -> raise <| OptimisticLockException ps
    
    member this.FindWithVersion<'T when 'T : not struct> idList (version : obj) : 'T = 
        this.FindCore<'T, 'T> idList (fun result versionPropMeta ps -> 
            match result with
            | Some entity -> 
                this.ValidateOptimisticLock version entity versionPropMeta ps
                entity
            | _ -> raise <| EntityNotFoundException ps)
    
    member this.TryFindWithVersion<'T when 'T : not struct> idList (version : obj) : 'T option = 
        this.FindCore<'T, 'T option> idList (fun result versionPropMeta ps -> 
            match result with
            | Some entity -> 
                this.ValidateOptimisticLock version entity versionPropMeta ps
                Some entity
            | _ -> None)
    
    member this.Execute sql exprCtxt : int = 
        let ps = Sql.prepare config sql exprCtxt config.SqlParser
        this.ExecuteNonQuery ps
    
    member this.RemakeEntity<'T> (entity : 'T, entityMeta : EntityMeta) propHandler = 
        let values = 
            entityMeta.PropMetaList
            |> Seq.map (fun propMeta -> propMeta, propMeta.GetValue(upcast entity))
            |> Seq.map propHandler
            |> Seq.toArray
        entityMeta.RemakeEntity (box entity) values :?> 'T
    
    member this.AppendPreparedStatements ps1 ps2 = 
        let text = ps1.Text + "; " + ps2.Text
        let formattedText = ps1.FormattedText + "; " + ps2.FormattedText
        let parameters = List.append (ps1.Parameters) (ps2.Parameters)
        { Text = text
          FormattedText = formattedText
          Parameters = parameters }
    
    member this.ExecuteAndGetFirst ps readerHandler = 
        let results = this.ExecuteReaderOnDemand ps (fun reader -> Seq.truncate 1 (readerHandler reader)) |> Seq.toList
        if results.IsEmpty then raise <| NoAffectedRowException ps
        else results.Head
    
    member this.PrepareVersionSelect entity (entityMeta : EntityMeta) (versionPropMeta : PropMeta) = 
        let idMetaList = 
            entityMeta.IdPropMetaList 
            |> List.map (fun propMeta -> propMeta.ColumnName, propMeta.GetValue(entity), propMeta.Type)
        dialect.PrepareVersionSelect(entityMeta.TableName, versionPropMeta.ColumnName, idMetaList)
    
    member this.ExecuteAndGetVersionAtOnce entity (entityMeta : EntityMeta) (versionPropMeta : PropMeta) ps = 
        let versionPs = this.PrepareVersionSelect entity entityMeta versionPropMeta
        let ps = this.AppendPreparedStatements ps versionPs
        
        let readerHandler (reader : DbDataReader) = 
            seq { 
                while reader.Read() do
                    yield dialect.GetValue(reader, 0, versionPropMeta.Property)
            }
        this.ExecuteAndGetFirst ps readerHandler
    
    member this.GetVersionOnly entity (entityMeta : EntityMeta) (versionPropMeta : PropMeta) = 
        let ps = this.PrepareVersionSelect entity entityMeta versionPropMeta
        
        let readerHandler (reader : DbDataReader) = 
            seq { 
                while reader.Read() do
                    yield dialect.GetValue(reader, 0, versionPropMeta.Property)
            }
        this.ExecuteAndGetFirst ps readerHandler
    
    member this.FailCauseOfTooManyAffectedRows ps rows = 
        raise <| Soma.Core.DbException(SR.SOMA4012(rows, ps.Text, ps.Parameters))
    
    member this.GetEntityMeta typ = 
        if not <| Meta.isEntityType typ then raise <| Soma.Core.DbException(SR.SOMA4007())
        Meta.makeEntityMeta typ dialect
    
    member this.ConvertFromColumnToPropIfNecessary (dbValueMap : Map<int, obj>) (propMeta : PropMeta, value) = 
        match dbValueMap.TryFind propMeta.Index with
        | Some dbValue -> Changed(this.ConvertFromColumnToProp propMeta dbValue)
        | _ -> Unchanged value
    
    member this.PreInsert<'T> (entity : 'T) (entityMeta : EntityMeta) = 
        let (|Sequence|_|) = 
            function 
            | GetSequenceAndInitVersion(idPropMeta, sequenceMeta, _) | GetSequence(idPropMeta, sequenceMeta) -> 
                let ps = dialect.PrepareSequenceSelect(sequenceMeta.SqlSequenceName)
                let dbValue = sequenceMeta.Generate config.ConnectionString (fun () -> this.ExecuteScalar ps)
                let value = this.ConvertFromColumnToProp idPropMeta dbValue
                Some(value, idPropMeta)
            | _ -> None
        
        let (|Version|_|) = 
            function 
            | GetSequenceAndInitVersion(_, _, versionPropMeta) | InitVersion(versionPropMeta) -> 
                let value = versionPropMeta.GetValue(upcast entity)
                let typ = versionPropMeta.Type
                if Reflection.lessThan (value, typ, 1) then Some(Reflection.one typ, versionPropMeta)
                else None
            | _ -> None
        
        match entityMeta.PreInsertCase with
        | Some preInsertCase -> 
            match preInsertCase with
            | Sequence(idValue, idPropMeta) & Version(versionValue, versionPropMeta) -> 
                this.RemakeEntity<'T> (entity, entityMeta) (fun (propMeta : PropMeta, value) -> 
                    if propMeta.Index = idPropMeta.Index then Changed idValue
                    elif propMeta.Index = versionPropMeta.Index then Changed versionValue
                    else Unchanged value)
            | Sequence(idValue, idPropMeta) -> 
                this.RemakeEntity<'T> (entity, entityMeta) (fun (propMeta : PropMeta, value) -> 
                    if propMeta.Index = idPropMeta.Index then Changed idValue
                    else Unchanged value)
            | Version(versionValue, versionPropMeta) -> 
                this.RemakeEntity<'T> (entity, entityMeta) (fun (propMeta : PropMeta, value) -> 
                    if propMeta.Index = versionPropMeta.Index then Changed versionValue
                    else Unchanged value)
            | _ -> entity
        | _ -> entity
    
    member this.Insert<'T when 'T : not struct>(entity : 'T, ?opt : InsertOpt) = 
        let entityMeta = this.GetEntityMeta typeof<'T>
        let entity = this.PreInsert entity entityMeta
        let opt = defaultArg opt (InsertOpt())
        let ps = Sql.prepareInsert config entity entityMeta opt
        let makeEntity dbValueMap = 
            this.RemakeEntity<'T> (entity, entityMeta) (this.ConvertFromColumnToPropIfNecessary dbValueMap)
        
        let insert() = 
            let rows = this.ExecuteNonQuery ps
            if rows < 1 then raise <| NoAffectedRowException ps
            else 
                if 1 < rows then this.FailCauseOfTooManyAffectedRows ps rows
        match entityMeta.InsertCase with
        | InsertThenGetIdentityAndVersionAtOnce(idPropMeta, versionPropMeta) -> 
            let identityPs = 
                dialect.PrepareIdentityAndVersionSelect
                    (entityMeta.TableName, idPropMeta.ColumnName, versionPropMeta.ColumnName)
            let ps = this.AppendPreparedStatements ps identityPs
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, idPropMeta.Property), 
                              dialect.GetValue(reader, 1, versionPropMeta.Property)
                }
            
            let idValue, versionValue = this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ idPropMeta.Index, idValue
                                versionPropMeta.Index, versionValue ]
        | InsertThenGetIdentityAtOnce(idPropMeta) -> 
            let identityPs = dialect.PrepareIdentitySelect(entityMeta.TableName, idPropMeta.ColumnName)
            let ps = this.AppendPreparedStatements ps identityPs
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, idPropMeta.Property)
                }
            
            let idValue = this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ idPropMeta.Index, idValue ]
        | InsertThenGetVersionAtOnce(versionPropMeta) -> 
            let versionValue = this.ExecuteAndGetVersionAtOnce entity entityMeta versionPropMeta ps
            makeEntity <| map [ versionPropMeta.Index, versionValue ]
        | InsertThenGetIdentityAndVersionLater(idPropMeta, versionPropMeta) -> 
            insert()
            let ps = 
                dialect.PrepareIdentityAndVersionSelect
                    (entityMeta.TableName, idPropMeta.ColumnName, versionPropMeta.ColumnName)
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, idPropMeta.Property), 
                              dialect.GetValue(reader, 1, versionPropMeta.Property)
                }
            
            let idValue, versionValue = id this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ idPropMeta.Index, idValue
                                versionPropMeta.Index, versionValue ]
        | InsertThenGetIdentityLater(idPropMeta) -> 
            insert()
            let ps = dialect.PrepareIdentitySelect(entityMeta.TableName, idPropMeta.ColumnName)
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, idPropMeta.Property)
                }
            
            let idValue = this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ idPropMeta.Index, idValue ]
        | InsertThenGetVersionLater(versionPropMeta) -> 
            insert()
            let versionValue = this.GetVersionOnly entity entityMeta versionPropMeta
            makeEntity <| map [ versionPropMeta.Index, versionValue ]
        | InsertOnly -> 
            insert()
            entity
    
    member this.Update<'T when 'T : not struct>(entity : 'T, ?opt : UpdateOpt) = 
        let typ = typeof<'T>
        let entityMeta = this.GetEntityMeta typ
        if entityMeta.IdPropMetaList.IsEmpty then raise <| Soma.Core.DbException(SR.SOMA4005(typ.FullName))
        let opt = defaultArg opt (UpdateOpt())
        let ps = Sql.prepareUpdate config entity entityMeta opt
        
        let update() = 
            let rows = this.ExecuteNonQuery ps
            if rows < 1 then 
                if opt.IgnoreVersion || entityMeta.VersionPropMeta.IsNone then raise <| NoAffectedRowException ps
                else raise <| OptimisticLockException ps
            if 1 < rows then this.FailCauseOfTooManyAffectedRows ps rows
        match entityMeta.UpdateCase with
        | UpdateThenGetVersionAtOnce versionPropMeta -> 
            let versionValue = this.ExecuteAndGetVersionAtOnce entity entityMeta versionPropMeta ps
            let dbValueMap = map [ versionPropMeta.Index, versionValue ]
            this.RemakeEntity<'T> (entity, entityMeta) (this.ConvertFromColumnToPropIfNecessary dbValueMap)
        | UpdateThenGetVersionLater versionPropMeta -> 
            update()
            let versionValue = this.GetVersionOnly entity entityMeta versionPropMeta
            let dbValueMap = map [ versionPropMeta.Index, versionValue ]
            this.RemakeEntity<'T> (entity, entityMeta) (this.ConvertFromColumnToPropIfNecessary dbValueMap)
        | UpdateThenIncrementVersion versionPropMeta -> 
            update()
            this.RemakeEntity<'T> (entity, entityMeta) (fun (propMeta : PropMeta, value) -> 
                if propMeta.Index = versionPropMeta.Index then 
                    let typ = propMeta.Type
                    Changed(Reflection.incr (value, typ))
                else Unchanged value)
        | UpdateOnly -> 
            update()
            entity
    
    member this.InsertOrUpdate<'T when 'T : not struct>(entity : 'T, ?opt : UpdateOpt) = 
        let entityMeta = this.GetEntityMeta typeof<'T>
        let insertEntity = this.PreInsert entity entityMeta
        let updateEntity = entity
        let opt = defaultArg opt (UpdateOpt())
        let ps = Sql.prepareInsertOrUpdate config insertEntity updateEntity entityMeta opt
        let makeEntity dbValueMap = 
            this.RemakeEntity<'T> (entity, entityMeta) (this.ConvertFromColumnToPropIfNecessary dbValueMap)
        
        let insert() = 
            let ps = ps None None
            let rows = this.ExecuteNonQuery ps
            if rows < 1 then raise <| NoAffectedRowException ps
            else 
                if 1 < rows then this.FailCauseOfTooManyAffectedRows ps rows
        match entityMeta.InsertOrUpdateCase with
        | InsertThenGetIdentityAndVersionAtOnce(idPropMeta, versionPropMeta) -> 
            let ps = ps (Some idPropMeta) (Some versionPropMeta)
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, idPropMeta.Property), 
                              dialect.GetValue(reader, 1, versionPropMeta.Property)
                }
            
            let idValue, versionValue = this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ idPropMeta.Index, idValue
                                versionPropMeta.Index, versionValue ]
        | InsertThenGetIdentityAtOnce(idPropMeta) -> 
            let ps = ps (Some idPropMeta) (None)
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, idPropMeta.Property)
                }
            
            let idValue = this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ idPropMeta.Index, idValue ]
        | InsertThenGetVersionAtOnce(versionPropMeta) -> 
            let ps = ps (Some versionPropMeta) (None)
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, versionPropMeta.Property)
                }
            
            let idValue = this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ versionPropMeta.Index, idValue ]
        | InsertThenGetIdentityAndVersionLater(idPropMeta, versionPropMeta) -> 
            insert()
            let ps = 
                dialect.PrepareIdentityAndVersionSelect
                    (entityMeta.TableName, idPropMeta.ColumnName, versionPropMeta.ColumnName)
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, idPropMeta.Property), 
                              dialect.GetValue(reader, 1, versionPropMeta.Property)
                }
            
            let idValue, versionValue = id this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ idPropMeta.Index, idValue
                                versionPropMeta.Index, versionValue ]
        | InsertThenGetIdentityLater(idPropMeta) -> 
            insert()
            let ps = dialect.PrepareIdentitySelect(entityMeta.TableName, idPropMeta.ColumnName)
            
            let readerHandler (reader : DbDataReader) = 
                seq { 
                    while reader.Read() do
                        yield dialect.GetValue(reader, 0, idPropMeta.Property)
                }
            
            let idValue = this.ExecuteAndGetFirst ps readerHandler
            makeEntity <| map [ idPropMeta.Index, idValue ]
        | InsertThenGetVersionLater(versionPropMeta) -> 
            insert()
            let versionValue = this.GetVersionOnly entity entityMeta versionPropMeta
            makeEntity <| map [ versionPropMeta.Index, versionValue ]
        | InsertOnly -> 
            insert()
            entity
    
    member this.Delete<'T when 'T : not struct>(entity : 'T, ?opt : DeleteOpt) = 
        let typ = typeof<'T>
        let entityMeta = this.GetEntityMeta typ
        checkCanDelete entityMeta
        let opt = defaultArg opt (DeleteOpt())
        let ps = Sql.prepareDelete config entity entityMeta opt
        let rows = this.ExecuteNonQuery ps
        if rows < 1 then 
            if opt.IgnoreVersion || entityMeta.VersionPropMeta.IsNone then raise <| NoAffectedRowException ps
            else raise <| OptimisticLockException ps
        if 1 < rows then this.FailCauseOfTooManyAffectedRows ps rows
    
    member this.Call<'T when 'T : not struct>(procedure : 'T) = 
        let typ = typeof<'T>
        
        let procedureMeta = 
            if not <| Meta.isProcedureType typ then raise <| Soma.Core.DbException(SR.SOMA4021())
            Meta.makeProcedureMeta typ dialect
        
        let ps = Sql.prepareCall config procedure procedureMeta
        
        let convertFromDbToClr dbValue (paramMeta : ProcedureParamMeta) = 
            this.ConvertFromDbToClr dbValue paramMeta.Type paramMeta.UdtTypeName paramMeta.Property 
                (fun exn -> 
                let typ = 
                    if dbValue = null then typeof<obj>
                    else dbValue.GetType()
                raise 
                <| Soma.Core.DbException
                       (SR.SOMA4023
                            (typ.FullName, paramMeta.ParamName, procedureMeta.ProcedureName, paramMeta.Type.FullName), 
                        exn))
        this.ExecuteCommand ps (fun command -> 
            command.CommandType <- CommandType.StoredProcedure
            let procedureAry = Array.zeroCreate (procedureMeta.ProcedureParamMetaList.Length)
            using (this.NotifyCommandExecute command (fun command -> command.ExecuteReader())) (fun reader -> 
                try 
                    procedureMeta.ProcedureParamMetaList
                    |> Seq.fold (fun (hasNextResult, reader) paramMeta -> 
                           match paramMeta.ParamMetaCase with
                           | Result(elementCase, typeConverter) -> 
                               if hasNextResult then 
                                   let resultList = 
                                       match elementCase with
                                       | EntityType entityMeta -> this.MakeEntityList entityMeta reader
                                       | TupleType tupleMeta -> this.MakeTupleList tupleMeta reader
                                   procedureAry.[paramMeta.Index] <- Changed(typeConverter resultList)
                               reader.NextResult(), reader
                           | _ -> hasNextResult, reader) (true, reader)
                    |> ignore
                finally
                    try 
                        while reader.NextResult() do
                            ()
                    with _ -> ())
            procedureMeta.ProcedureParamMetaList
            |> Seq.filter (fun paramMeta -> 
                   match paramMeta.ParamMetaCase with
                   | Result _ -> false
                   | _ -> true)
            |> Seq.iter (fun paramMeta -> 
                   let paramName = dialect.CreateParameterName paramMeta.ParamName
                   
                   let valueCase = 
                       if command.Parameters.Contains(paramName) then 
                           let value = command.Parameters.[paramName].Value
                           match paramMeta.ParamMetaCase with
                           | Unit -> failwith "unreachable."
                           | Input -> Unchanged value
                           | _ -> Changed(convertFromDbToClr value paramMeta)
                       else Unchanged null
                   procedureAry.[paramMeta.Index] <- valueCase)
            procedureMeta.RemakeProcedure (box procedure) procedureAry :?> 'T)
    
    member this.Queryable<'T when 'T : not struct>() : System.Linq.IQueryable<'T> = 
        let queryProvider = queryProvider.Force()
        FSharp.QueryProvider.Queryable.Query<'T>(queryProvider, None) :> System.Linq.IQueryable<'T>
    
    member this.QueryableDelete<'T when 'T : not struct>(query : System.Linq.IQueryable<'T>) : unit = 
        let typ = typeof<'T>
        let entityMeta = this.GetEntityMeta typ
        //checkCanDelete entityMeta
        use connection = config.DbProviderFactory.CreateConnection()
        this.SetupConnection connection
        this.NotifyConnectionOpen connection (fun connection -> connection.Open())
        let command, _ = config.QueryTranslator DeleteQuery connection query.Expression
        let rows = this.NotifyCommandExecute command (fun command -> command.ExecuteNonQuery())
        if rows < 1 then 
            failwith "NoAffectedRowException" //convert this to return a result record
        if 1 < rows then 
            failwith "this.FailCauseOfTooManyAffectedRows ps rows" //convert this to return a result record
