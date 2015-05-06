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
open FSharp.QueryProvider

[<AbstractClass>]
type DbConfigBase(invariant : string) = 
    do Guard.argNotNull (invariant, "invariant")
    let dbProviderFactory = DbProviderFactories.GetFactory(invariant)
    let statementCache = ConcurrentDictionary<string, Lazy<SqlAst.Statement>>()
    let expressionCache = ConcurrentDictionary<string, Lazy<ExpressionAst.Expression>>()
    
    let connectionObserver = 
        { new IConnectionObserver with
              member this.NotifyOpening(connection : DbConnection, 
                                        [<System.Runtime.InteropServices.Out>] userState : byref<obj>) = ()
              member this.NotifyOpened(connection : DbConnection, userState : obj) = () }
    
    let commandObserver = 
        { new ICommandObserver with
              member this.NotifyExecuting(command : IDbCommand, 
                                          [<System.Runtime.InteropServices.Out>] userState : byref<obj>) = ()
              member this.NotifyExecuted(command : IDbCommand, userState : obj) = () }
    
    member this.CacheSqlParser = 
        Func<string, SqlAst.Statement>(fun sql -> statementCache.GetOrAdd(sql, Lazy(fun () -> Sql.parse sql)).Value)
    member this.NoCacheSqlParser = Func<string, SqlAst.Statement>(fun sql -> Sql.parse sql)
    member this.CacheExpressionParser = 
        Func<string, ExpressionAst.Expression>
            (fun expr -> expressionCache.GetOrAdd(expr, Lazy(fun () -> Expression.parse expr)).Value)
    member this.NoCacheExpressionParser = Func<string, ExpressionAst.Expression>(fun expr -> Expression.parse expr)
    member this.ConsoleLogger = 
        Action<PreparedStatement>
            (fun preparedStatement -> Console.WriteLine("LOG : " + preparedStatement.FormattedText))
    member this.SilentLogger = Action<PreparedStatement>(fun _ -> ())
    member this.Invariant = invariant
    member this.DbProviderFactory = dbProviderFactory
    abstract ConnectionString : string
    abstract Dialect : IDialect
    abstract QueryTranslator : 
        QueryType -> 
        IDbConnection -> 
        System.Linq.Expressions.Expression -> 
        IDbCommand * FSharp.QueryProvider.DataReader.TypeConstructionInfo option
    abstract SqlParser : Func<string, SqlAst.Statement>
    override this.SqlParser = this.CacheSqlParser
    abstract ExpressionParser : Func<string, ExpressionAst.Expression>
    override this.ExpressionParser = this.CacheExpressionParser
    abstract Logger : Action<PreparedStatement>
    override this.Logger = this.ConsoleLogger
    abstract ConnectionObserver : IConnectionObserver
    override this.CommandObserver = commandObserver
    abstract CommandObserver : ICommandObserver
    override this.ConnectionObserver = connectionObserver
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
        member this.QueryTranslator qt c e = this.QueryTranslator qt c e

module private utils =
    let getTableName dialect = 
        Some(fun typ -> 
            let entityMeta = Meta.makeEntityMeta typ dialect
            Some entityMeta.TableName)
        
    let getColumnName dialect = 
        Some(fun (memberInfo : System.Reflection.MemberInfo) -> 
            let entityMeta = Meta.makeEntityMeta memberInfo.DeclaringType dialect
            let propMeta = entityMeta.PropMetaList |> Seq.find (fun p -> p.PropName = memberInfo.Name)
            Some propMeta.ColumnName)

    //let translate queryDialect somaDialect queryType connection expresssion =
        

[<AbstractClass>]
type MsSqlConfig() = 
    inherit DbConfigBase("System.Data.SqlClient")
    static let dialect = MsSqlDialect() :> IDialect
    override this.Dialect = dialect
    override this.QueryTranslator queryType connection expression = 
        
        let queryType = 
            match queryType with
            | DeleteQuery -> QueryTranslator.DeleteQuery
            | SelectQuery -> QueryTranslator.SelectQuery
        let q, c = 
            QueryTranslator.translateToCommand QueryTranslator.SqlServer2012 queryType None (utils.getTableName dialect) (utils.getColumnName dialect) (connection :?> SqlClient.SqlConnection) expression
        q :> IDbCommand, c

[<AbstractClass>]
type MsSqlCeConfig() = 
    inherit DbConfigBase("System.Data.SqlServerCe.4.0")
    static let dialect = MsSqlCeDialect() :> IDialect
    override this.Dialect = dialect
    override this.QueryTranslator queryType connection expression = failwith "not implemented"

//    let q,c = FSharp.QueryProvider.Engines.SqlServer.translateToCommand None None None (connection :?> SqlClient.SqlConnection) expression
//    q :> IDbCommand, c
[<AbstractClass>]
type MySqlConfig() = 
    inherit DbConfigBase("MySql.Data.MySqlClient")
    static let dialect = MySqlDialect() :> IDialect
    override this.Dialect = dialect
    override this.QueryTranslator queryType connection expression = failwith "not implemented"

//    let q,c = FSharp.QueryProvider.Engines.SqlServer.translateToCommand None None None (connection :?> SqlClient.SqlConnection) expression
//    q :> IDbCommand, c
[<AbstractClass>]
type OracleConfig() = 
    inherit DbConfigBase("Oracle.DataAccess.Client")
    static let dialect = OracleDialect() :> IDialect
    override this.Dialect = dialect
    override this.QueryTranslator queryType connection expression = failwith "not implemented"

//    let q,c = FSharp.QueryProvider.Engines.SqlServer.translateToCommand None None None (connection :?> SqlClient.SqlConnection) expression
//    q :> IDbCommand, c
[<AbstractClass>]
type SQLiteConfig() = 
    inherit DbConfigBase("System.Data.SQLite")
    static let dialect = SQLiteDialect() :> IDialect
    override this.Dialect = dialect
    override this.QueryTranslator queryType connection expression = failwith "not implemented"

//    let q,c = FSharp.QueryProvider.Engines.SqlServer.translateToCommand None None None (connection :?> SqlClient.SqlConnection) expression
//    q :> IDbCommand, c
type PlainConfig(invariant : string, connectionString : string, dialect : IDialect, queryTranslator) = 
    inherit DbConfigBase(invariant)
    let mutable logger : Action<PreparedStatement> = base.SilentLogger
    override this.ConnectionString = connectionString
    override this.Dialect = dialect
    override this.QueryTranslator queryType connection e = queryTranslator connection e
    override this.Logger = logger
    member this.SetLogger(value) = logger <- value

type LocalDbImpl(config : IDbConfig, connection : DbConnection) = 
    inherit DbImpl(config)
    override this.ExecuteCommandOnDemand (ps : PreparedStatement) commandHandler = 
        seq { 
            use command = connection.CreateCommand()
            use paramsDisposer = this.SetupCommand ps command
            config.Logger.Invoke ps
            if connection.State = ConnectionState.Closed then 
                this.NotifyConnectionOpen connection (fun connection -> connection.Open())
            yield! this.HandleCommand ps command commandHandler
        }

[<RequireQualifiedAccess>]
module Conversion = 
    let toIdList (id : obj) = 
        match id with
        | :? string -> [ id ]
        | :? IEnumerable as enumerable -> 
            enumerable
            |> Seq.cast<obj>
            |> Seq.toList
        | _ -> [ id ]
    
    let fromListToExprCtxt (condition : (string * obj * Type) list) = 
        let dict = Dictionary<string, obj * Type>() :> IDictionary<string, obj * Type>
        condition |> List.iter (fun (key, value, typ) -> 
                         if dict.ContainsKey(key) then dict.Remove(key) |> ignore
                         dict.[key] <- (value, typ))
        dict
    
    let toExprCtxt (condition : obj) = 
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
            (keys, values) ||> Seq.iter2 (fun key value -> dict.[string key] <- (value, typeof<obj>))
            dict
        else 
            match condition with
            | :? list<string * obj * Type> as list -> fromListToExprCtxt list
            | :? IDictionary as d -> 
                let dict = Dictionary<string, obj * Type>(d.Count) :> IDictionary<string, obj * Type>
                d.Keys
                |> Seq.cast<obj>
                |> Seq.iter (fun key -> dict.[string key] <- (d.[key], typeof<obj>))
                dict
            | _ -> raise <| Soma.Core.DbException(SR.SOMA4020())

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
    abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : obj * obj -> 'T
    abstract Insert<'T when 'T : not struct> : 'T -> unit
    abstract Insert<'T when 'T : not struct> : 'T * opt:InsertOpt -> unit
    abstract Update<'T when 'T : not struct> : 'T -> unit
    abstract Update<'T when 'T : not struct> : 'T * opt:UpdateOpt -> unit
    abstract InsertOrUpdate<'T when 'T : not struct> : 'T -> unit
    abstract Delete<'T when 'T : not struct> : 'T -> unit
    abstract Delete<'T when 'T : not struct> : 'T * opt:DeleteOpt -> unit
    abstract Call<'T when 'T : not struct> : 'T -> unit
    abstract Queryable<'T when 'T : not struct> : unit -> System.Linq.IQueryable<'T>
    abstract QueryableDelete<'T when 'T : not struct> : System.Linq.IQueryable<'T> -> unit

type Db(config : IDbConfig) = 
    do Guard.argNotNull (config, "config")
    let db = new DbImpl(config)
    abstract DbConfig : IDbConfig
    override this.DbConfig = config
    abstract Query<'T> : sql:string -> 'T IList
    
    override this.Query<'T>(sql : string) = 
        Guard.argNotNull (sql, "sql")
        let seq = db.QueryOnDemand<'T> sql Map.empty
        ResizeArray(seq) :> IList<'T>
    
    abstract Query<'T> : sql:string * condition:obj -> 'T IList
    
    override this.Query<'T>(sql : string, condition : obj) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let seq = db.QueryOnDemand<'T> sql exprCtxt
        ResizeArray(seq) :> IList<'T>
    
    abstract QueryOnDemand<'T> : sql:string -> 'T seq
    
    override this.QueryOnDemand<'T>(sql : string) = 
        Guard.argNotNull (sql, "sql")
        db.QueryOnDemand<'T> sql Map.empty
    
    abstract QueryOnDemand<'T> : sql:string * condition:obj -> 'T seq
    
    override this.QueryOnDemand<'T>(sql : string, condition : obj) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        db.QueryOnDemand<'T> sql exprCtxt
    
    abstract Paginate<'T> : sql:string * offset:int64 * limit:int64 -> 'T IList
    
    override this.Paginate<'T>(sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (sql, "sql")
        let seq = db.PaginateOnDemand<'T> sql Map.empty (offset, limit)
        ResizeArray(seq) :> IList<'T>
    
    abstract Paginate<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList
    
    override this.Paginate<'T>(sql : string, offset : int64, limit : int64, condition : obj) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let seq = db.PaginateOnDemand<'T> sql exprCtxt (offset, limit)
        ResizeArray(seq) :> IList<'T>
    
    abstract PaginateOnDemand<'T> : sql:string * offset:int64 * limit:int64 -> 'T seq
    
    override this.PaginateOnDemand<'T>(sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (sql, "sql")
        db.PaginateOnDemand<'T> sql Map.empty (offset, limit)
    
    abstract PaginateOnDemand<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T seq
    
    override this.PaginateOnDemand<'T>(sql : string, offset : int64, limit : int64, condition : obj) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        db.PaginateOnDemand<'T> sql exprCtxt (offset, limit)
    
    abstract PaginateAndCount<'T> : sql:string * offset:int64 * limit:int64 -> 'T IList * int64
    
    override this.PaginateAndCount<'T>(sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (sql, "sql")
        let list, count = db.PaginateAndCount<'T> sql Map.empty (offset, limit)
        ResizeArray(list) :> IList<'T>, count
    
    abstract PaginateAndCount<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList * int64
    
    override this.PaginateAndCount<'T>(sql : string, offset : int64, limit : int64, condition : obj) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let list, count = db.PaginateAndCount<'T> sql exprCtxt (offset, limit)
        ResizeArray(list) :> IList<'T>, count
    
    abstract Execute : sql:string -> int
    
    override this.Execute(sql : string) = 
        Guard.argNotNull (sql, "sql")
        db.Execute sql Map.empty
    
    abstract Execute : sql:string * condition:obj -> int
    
    override this.Execute(sql : string, condition : obj) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        db.Execute sql exprCtxt
    
    abstract ExecuteReader<'T> : handler:Func<DbDataReader, 'T> * sql:string -> 'T
    
    override this.ExecuteReader<'T>(handler : Func<DbDataReader, 'T>, sql : string) = 
        Guard.argNotNull (handler, "handler")
        Guard.argNotNull (sql, "sql")
        db.ExecuteReader<'T> (fun reader -> handler.Invoke reader) sql Map.empty
    
    abstract ExecuteReader<'T> : handler:Func<DbDataReader, 'T> * sql:string * condition:obj -> 'T
    
    override this.ExecuteReader<'T>(handler : Func<DbDataReader, 'T>, sql : string, condition : obj) = 
        Guard.argNotNull (handler, "handler")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        db.ExecuteReader<'T> (fun reader -> handler.Invoke reader) sql exprCtxt
    
    abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : id:obj -> 'T
    
    override this.Find<'T when 'T : not struct and 'T : (new : unit -> 'T)>(id : obj) = 
        Guard.argNotNull (id, "id")
        db.Find<'T>(Conversion.toIdList id)
    
    abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : id:obj -> 'T
    
    override this.TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(id : obj) = 
        Guard.argNotNull (id, "id")
        match db.TryFind<'T>(Conversion.toIdList id) with
        | Some found -> found
        | _ -> null
    
    abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : id:obj * version:obj -> 'T
    
    override this.FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)>(id : obj, version : obj) = 
        Guard.argNotNull (id, "id")
        Guard.argNotNull (version, "version")
        db.FindWithVersion<'T> (Conversion.toIdList id) version
    
    abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : id:obj * version:obj
     -> 'T
    
    override this.TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(id : obj, 
                                                                                                        version : obj) = 
        Guard.argNotNull (id, "id")
        Guard.argNotNull (version, "version")
        match db.TryFindWithVersion<'T> (Conversion.toIdList id) version with
        | Some entity -> entity
        | _ -> null
    
    abstract Insert<'T when 'T : not struct> : entity:'T -> unit
    
    override this.Insert<'T when 'T : not struct>(entity) = 
        Guard.argNotNull (entity, "entity")
        db.Insert<'T> entity |> ignore
    
    abstract Insert<'T when 'T : not struct> : entity:'T * opt:InsertOpt -> unit
    
    override this.Insert<'T when 'T : not struct>(entity, opt) = 
        Guard.argNotNull (entity, "entity")
        Guard.argNotNull (opt, "opt")
        db.Insert<'T>(entity, opt) |> ignore
    
    abstract Update<'T when 'T : not struct> : entity:'T -> unit
    
    override this.Update<'T when 'T : not struct>(entity) = 
        Guard.argNotNull (entity, "entity")
        db.Update<'T> entity |> ignore
    
    abstract Update<'T when 'T : not struct> : entity:'T * opt:UpdateOpt -> unit
    
    override this.Update<'T when 'T : not struct>(entity, opt) = 
        Guard.argNotNull (entity, "entity")
        Guard.argNotNull (opt, "opt")
        db.Update<'T>(entity, opt) |> ignore
    
    abstract InsertOrUpdate<'T when 'T : not struct> : entity:'T -> unit
    
    override this.InsertOrUpdate<'T when 'T : not struct>(entity) = 
        Guard.argNotNull (entity, "entity")
        db.InsertOrUpdate<'T>(entity) |> ignore
    
    abstract Delete<'T when 'T : not struct> : entity:'T -> unit
    
    override this.Delete<'T when 'T : not struct>(entity) = 
        Guard.argNotNull (entity, "entity")
        db.Delete<'T> entity
    
    abstract Delete<'T when 'T : not struct> : entity:'T * opt:DeleteOpt -> unit
    
    override this.Delete<'T when 'T : not struct>(entity, opt) = 
        Guard.argNotNull (entity, "entity")
        Guard.argNotNull (opt, "opt")
        db.Delete<'T>(entity, opt)
    
    abstract Call<'T when 'T : not struct> : procedure:'T -> unit
    
    override this.Call<'T when 'T : not struct>(procedure) = 
        Guard.argNotNull (procedure, "procedure")
        db.Call<'T> procedure |> ignore
    
    abstract Queryable<'T when 'T : not struct> : unit -> System.Linq.IQueryable<'T>
    override this.Queryable<'T when 'T : not struct>() = db.Queryable<'T>()
    abstract QueryableDelete<'T when 'T : not struct> : System.Linq.IQueryable<'T> -> unit
    override this.QueryableDelete<'T when 'T : not struct>(query : System.Linq.IQueryable<'T>) = 
        db.QueryableDelete<'T>(query)
    interface IDb with
        member this.DbConfig = this.DbConfig
        member this.Query<'T>(sql : string) = this.Query<'T>(sql)
        member this.Query<'T>(sql : string, condition : obj) = this.Query<'T>(sql, condition)
        member this.QueryOnDemand<'T>(sql : string) = this.QueryOnDemand<'T>(sql)
        member this.QueryOnDemand<'T>(sql : string, condition : obj) = this.QueryOnDemand<'T>(sql, condition)
        member this.Paginate<'T>(sql : string, offset : int64, limit : int64) = this.Paginate<'T>(sql, offset, limit)
        member this.Paginate<'T>(sql : string, offset : int64, limit : int64, condition : obj) = 
            this.Paginate<'T>(sql, offset, limit, condition)
        member this.PaginateOnDemand<'T>(sql : string, offset : int64, limit : int64) = 
            this.PaginateOnDemand<'T>(sql, offset, limit)
        member this.PaginateOnDemand<'T>(sql : string, offset : int64, limit : int64, condition : obj) = 
            this.PaginateOnDemand<'T>(sql, offset, limit, condition)
        member this.PaginateAndCount<'T>(sql : string, offset : int64, limit : int64) = 
            this.PaginateAndCount<'T>(sql, offset, limit)
        member this.PaginateAndCount<'T>(sql : string, offset : int64, limit : int64, condition : obj) = 
            this.PaginateAndCount<'T>(sql, offset, limit, condition)
        member this.Execute(sql : string) = this.Execute(sql)
        member this.Execute(sql : string, condition : obj) = this.Execute(sql, condition)
        member this.ExecuteReader<'T>(handler : Func<DbDataReader, 'T>, sql : string) = this.ExecuteReader(handler, sql)
        member this.ExecuteReader<'T>(handler : Func<DbDataReader, 'T>, sql : string, condition : obj) = 
            this.ExecuteReader(handler, sql, condition)
        member this.Find<'T when 'T : not struct and 'T : (new : unit -> 'T)>(id : obj) = this.Find<'T>(id)
        member this.TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(id : obj) = 
            this.TryFind<'T>(id)
        member this.FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)>(id : obj, version : obj) = 
            this.FindWithVersion<'T>(id, version)
        member this.TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(id : obj, 
                                                                                                          version : obj) = 
            this.TryFindWithVersion<'T>(id, version)
        member this.Insert<'T when 'T : not struct>(entity : 'T) = this.Insert<'T>(entity)
        member this.Insert<'T when 'T : not struct>(entity : 'T, opt : InsertOpt) = this.Insert<'T>(entity, opt)
        member this.Update<'T when 'T : not struct>(entity : 'T) = this.Update(entity)
        member this.Update<'T when 'T : not struct>(entity : 'T, opt : UpdateOpt) = this.Update(entity, opt)
        member this.InsertOrUpdate<'T when 'T : not struct>(entity : 'T) = this.InsertOrUpdate(entity)
        member this.Delete<'T when 'T : not struct>(entity : 'T) = this.Delete<'T>(entity)
        member this.Delete<'T when 'T : not struct>(entity : 'T, opt : DeleteOpt) = this.Delete<'T>(entity, opt)
        member this.Call<'T when 'T : not struct>(procedure) = this.Call<'T>(procedure)
        member this.Queryable<'T when 'T : not struct>() : System.Linq.IQueryable<'T> = this.Queryable<'T>()
        member this.QueryableDelete<'T when 'T : not struct>(query : System.Linq.IQueryable<'T>) = 
            this.QueryableDelete<'T>(query)

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
    abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : DbConnection * obj * obj
     -> 'T
    abstract Insert<'T when 'T : not struct> : DbConnection * 'T -> unit
    abstract Insert<'T when 'T : not struct> : DbConnection * 'T * opt:InsertOpt -> unit
    abstract Update<'T when 'T : not struct> : DbConnection * 'T -> unit
    abstract Update<'T when 'T : not struct> : DbConnection * 'T * opt:UpdateOpt -> unit
    abstract InsertOrUpdate<'T when 'T : not struct> : DbConnection * 'T -> unit
    abstract Delete<'T when 'T : not struct> : DbConnection * 'T -> unit
    abstract Delete<'T when 'T : not struct> : DbConnection * 'T * opt:DeleteOpt -> unit
    abstract Call<'T when 'T : not struct> : DbConnection * 'T -> unit
    abstract CreateConnection : unit -> DbConnection
    abstract Queryable<'T when 'T : not struct> : connection:DbConnection -> System.Linq.IQueryable<'T>
    abstract QueryableDelete<'T when 'T : not struct> : connection:DbConnection * query:System.Linq.IQueryable<'T>
     -> unit

type LocalDb(config : IDbConfig) = 
    do Guard.argNotNull (config, "config")
    abstract DbConfig : IDbConfig
    override this.DbConfig = config
    abstract Query<'T> : connection:DbConnection * sql:string -> 'T IList
    
    override this.Query<'T>(connection : DbConnection, sql : string) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        let db = LocalDbImpl(config, connection)
        let seq = db.QueryOnDemand<'T> sql Map.empty
        ResizeArray(seq) :> IList<'T>
    
    abstract Query<'T> : connection:DbConnection * sql:string * condition:obj -> 'T IList
    
    override this.Query<'T>(connection : DbConnection, sql : string, condition : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let db = LocalDbImpl(config, connection)
        let seq = db.QueryOnDemand<'T> sql exprCtxt
        ResizeArray(seq) :> IList<'T>
    
    abstract QueryOnDemand<'T> : connection:DbConnection * sql:string -> 'T seq
    
    override this.QueryOnDemand<'T>(connection : DbConnection, sql : string) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        let db = LocalDbImpl(config, connection)
        db.QueryOnDemand<'T> sql Map.empty
    
    abstract QueryOnDemand<'T> : connection:DbConnection * sql:string * condition:obj -> 'T seq
    
    override this.QueryOnDemand<'T>(connection : DbConnection, sql : string, condition : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let db = LocalDbImpl(config, connection)
        db.QueryOnDemand<'T> sql exprCtxt
    
    abstract Paginate<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T IList
    
    override this.Paginate<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        let db = LocalDbImpl(config, connection)
        let seq = db.PaginateOnDemand<'T> sql Map.empty (offset, limit)
        ResizeArray(seq) :> IList<'T>
    
    abstract Paginate<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj
     -> 'T IList
    
    override this.Paginate<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64, condition : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let db = LocalDbImpl(config, connection)
        let list = db.PaginateOnDemand<'T> sql exprCtxt (offset, limit)
        ResizeArray(list) :> IList<'T>
    
    abstract PaginateOnDemand<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T seq
    
    override this.PaginateOnDemand<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        let db = LocalDbImpl(config, connection)
        db.PaginateOnDemand<'T> sql Map.empty (offset, limit)
    
    abstract PaginateOnDemand<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj
     -> 'T seq
    
    override this.PaginateOnDemand<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64, 
                                       condition : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let db = LocalDbImpl(config, connection)
        db.PaginateOnDemand<'T> sql exprCtxt (offset, limit)
    
    abstract PaginateAndCount<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64
     -> 'T IList * int64
    
    override this.PaginateAndCount<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        let db = LocalDbImpl(config, connection)
        let list, count = db.PaginateAndCount<'T> sql Map.empty (offset, limit)
        ResizeArray(list) :> IList<'T>, count
    
    abstract PaginateAndCount<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj
     -> 'T IList * int64
    
    override this.PaginateAndCount<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64, 
                                       condition : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let db = LocalDbImpl(config, connection)
        let list, count = db.PaginateAndCount<'T> sql exprCtxt (offset, limit)
        ResizeArray(list) :> IList<'T>, count
    
    abstract Execute : connection:DbConnection * sql:string -> int
    
    override this.Execute(connection : DbConnection, sql : string) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        let db = LocalDbImpl(config, connection)
        db.Execute sql Map.empty
    
    abstract Execute : connection:DbConnection * sql:string * condition:obj -> int
    
    override this.Execute(connection : DbConnection, sql : string, condition : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let db = LocalDbImpl(config, connection)
        db.Execute sql exprCtxt
    
    abstract ExecuteReader<'T> : connection:DbConnection * handler:Func<DbDataReader, 'T> * sql:string -> 'T
    
    override this.ExecuteReader<'T>(connection : DbConnection, handler : Func<DbDataReader, 'T>, sql : string) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (handler, "handler")
        Guard.argNotNull (sql, "sql")
        let db = LocalDbImpl(config, connection)
        db.ExecuteReader (fun reader -> handler.Invoke reader) sql Map.empty
    
    abstract ExecuteReader : connection:DbConnection * handler:Func<DbDataReader, 'T> * sql:string * condition:obj -> 'T
    
    override this.ExecuteReader(connection : DbConnection, handler : Func<DbDataReader, 'T>, sql : string, 
                                condition : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (handler, "handler")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = Conversion.toExprCtxt condition
        let db = LocalDbImpl(config, connection)
        db.ExecuteReader (fun reader -> handler.Invoke reader) sql exprCtxt
    
    abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : connection:DbConnection * id:obj -> 'T
    
    override this.Find<'T when 'T : not struct and 'T : (new : unit -> 'T)>(connection : DbConnection, id : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (id, "id")
        let db = LocalDbImpl(config, connection)
        db.Find<'T>(Conversion.toIdList id)
    
    abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : connection:DbConnection * id:obj
     -> 'T
    
    override this.TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(connection : DbConnection, 
                                                                                             id : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (id, "id")
        let db = LocalDbImpl(config, connection)
        match db.TryFind<'T>(Conversion.toIdList id) with
        | Some found -> found
        | _ -> null
    
    abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : connection:DbConnection * id:obj * version:obj
     -> 'T
    
    override this.FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)>(connection : DbConnection, 
                                                                                       id : obj, version : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (id, "id")
        Guard.argNotNull (version, "version")
        let db = LocalDbImpl(config, connection)
        db.FindWithVersion<'T> (Conversion.toIdList id) version
    
    abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : connection:DbConnection * id:obj * version:obj
     -> 'T
    
    override this.TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(connection : DbConnection, 
                                                                                                        id : obj, 
                                                                                                        version : obj) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (id, "id")
        Guard.argNotNull (version, "version")
        let db = LocalDbImpl(config, connection)
        match db.TryFindWithVersion<'T> (Conversion.toIdList id) version with
        | Some entity -> entity
        | _ -> null
    
    abstract Insert<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit
    
    override this.Insert<'T when 'T : not struct>(connection : DbConnection, entity) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (entity, "entity")
        let db = LocalDbImpl(config, connection)
        db.Insert<'T> entity |> ignore
    
    abstract Insert<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:InsertOpt -> unit
    
    override this.Insert<'T when 'T : not struct>(connection : DbConnection, entity, opt) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (entity, "entity")
        Guard.argNotNull (opt, "opt")
        let db = LocalDbImpl(config, connection)
        db.Insert<'T>(entity, opt) |> ignore
    
    abstract Update<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit
    
    override this.Update<'T when 'T : not struct>(connection : DbConnection, entity) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (entity, "entity")
        let db = LocalDbImpl(config, connection)
        db.Update<'T> entity |> ignore
    
    abstract Update<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:UpdateOpt -> unit
    
    override this.Update<'T when 'T : not struct>(connection : DbConnection, entity, opt) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (entity, "entity")
        Guard.argNotNull (opt, "opt")
        let db = LocalDbImpl(config, connection)
        db.Update<'T>(entity, opt) |> ignore
    
    abstract InsertOrUpdate<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit
    
    override this.InsertOrUpdate<'T when 'T : not struct>(connection : DbConnection, entity) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (entity, "entity")
        let db = LocalDbImpl(config, connection)
        db.InsertOrUpdate<'T>(entity) |> ignore
    
    abstract Delete<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit
    
    override this.Delete<'T when 'T : not struct>(connection : DbConnection, entity) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (entity, "entity")
        let db = LocalDbImpl(config, connection)
        db.Delete<'T> entity
    
    abstract Delete<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:DeleteOpt -> unit
    
    override this.Delete<'T when 'T : not struct>(connection : DbConnection, entity, opt) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (entity, "entity")
        Guard.argNotNull (opt, "opt")
        let db = LocalDbImpl(config, connection)
        db.Delete<'T>(entity, opt)
    
    abstract Call<'T when 'T : not struct> : connection:DbConnection * procedure:'T -> unit
    
    override this.Call<'T when 'T : not struct>(connection : DbConnection, procedure) = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (procedure, "procedure")
        let db = LocalDbImpl(config, connection)
        db.Call<'T> procedure |> ignore
    
    abstract CreateConnection : unit -> DbConnection
    
    override this.CreateConnection() = 
        let connection = config.DbProviderFactory.CreateConnection()
        connection.ConnectionString <- config.ConnectionString
        connection
    
    abstract Queryable<'T when 'T : not struct> : connection:DbConnection -> System.Linq.IQueryable<'T>
    
    override this.Queryable<'T when 'T : not struct>(connection : DbConnection) = 
        Guard.argNotNull (connection, "connection")
        let db = LocalDbImpl(config, connection)
        db.Queryable<'T>()
    
    abstract QueryableDelete<'T when 'T : not struct> : connection:DbConnection * query:System.Linq.IQueryable<'T>
     -> unit
    
    override this.QueryableDelete<'T when 'T : not struct>(connection : DbConnection, query) = 
        Guard.argNotNull (connection, "connection")
        let db = LocalDbImpl(config, connection)
        db.QueryableDelete<'T>(query)
    
    interface ILocalDb with
        member this.DbConfig = this.DbConfig
        member this.Query<'T>(connection : DbConnection, sql : string) = this.Query<'T>(connection, sql)
        member this.Query<'T>(connection : DbConnection, sql : string, condition : obj) = 
            this.Query<'T>(connection, sql, condition)
        member this.QueryOnDemand<'T>(connection : DbConnection, sql : string) = this.QueryOnDemand<'T>(connection, sql)
        member this.QueryOnDemand<'T>(connection : DbConnection, sql : string, condition : obj) = 
            this.QueryOnDemand<'T>(connection, sql, condition)
        member this.Paginate<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64) = 
            this.Paginate<'T>(connection, sql, offset, limit)
        member this.Paginate<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64, condition : obj) = 
            this.Paginate<'T>(connection, sql, offset, limit, condition)
        member this.PaginateOnDemand<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64) = 
            this.PaginateOnDemand<'T>(connection, sql, offset, limit)
        member this.PaginateOnDemand<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64, 
                                         condition : obj) = 
            this.PaginateOnDemand<'T>(connection, sql, offset, limit, condition)
        member this.PaginateAndCount<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64) = 
            this.PaginateAndCount<'T>(connection, sql, offset, limit)
        member this.PaginateAndCount<'T>(connection : DbConnection, sql : string, offset : int64, limit : int64, 
                                         condition : obj) = 
            this.PaginateAndCount<'T>(connection, sql, offset, limit, condition)
        member this.Execute(connection : DbConnection, sql : string) = this.Execute(connection, sql)
        member this.Execute(connection : DbConnection, sql : string, condition : obj) = 
            this.Execute(connection, sql, condition)
        member this.ExecuteReader<'T>(connection : DbConnection, handler : Func<DbDataReader, 'T>, sql : string) = 
            this.ExecuteReader(connection, handler, sql)
        member this.ExecuteReader<'T>(connection : DbConnection, handler : Func<DbDataReader, 'T>, sql : string, 
                                      condition : obj) = this.ExecuteReader(connection, handler, sql, condition)
        member this.Find<'T when 'T : not struct and 'T : (new : unit -> 'T)>(connection : DbConnection, id : obj) = 
            this.Find<'T>(connection, id)
        member this.TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(connection : DbConnection, 
                                                                                               id : obj) = 
            this.TryFind<'T>(connection, id)
        member this.FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)>(connection : DbConnection, 
                                                                                         id : obj, version : obj) = 
            this.FindWithVersion<'T>(connection, id, version)
        member this.TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>(connection : DbConnection, 
                                                                                                          id : obj, 
                                                                                                          version : obj) = 
            this.TryFindWithVersion<'T>(connection, id, version)
        member this.Insert<'T when 'T : not struct>(connection : DbConnection, entity : 'T) = 
            this.Insert<'T>(connection, entity)
        member this.Insert<'T when 'T : not struct>(connection : DbConnection, entity : 'T, opt : InsertOpt) = 
            this.Insert<'T>(connection, entity, opt)
        member this.Update<'T when 'T : not struct>(connection : DbConnection, entity : 'T) = 
            this.Update<'T>(connection, entity)
        member this.Update<'T when 'T : not struct>(connection : DbConnection, entity : 'T, opt : UpdateOpt) = 
            this.Update<'T>(connection, entity, opt)
        member this.InsertOrUpdate<'T when 'T : not struct>(connection : DbConnection, entity : 'T) = 
            this.InsertOrUpdate<'T>(connection, entity)
        member this.Delete<'T when 'T : not struct>(connection : DbConnection, entity : 'T) = 
            this.Delete<'T>(connection, entity)
        member this.Delete<'T when 'T : not struct>(connection : DbConnection, entity : 'T, opt : DeleteOpt) = 
            this.Delete<'T>(connection, entity, opt)
        member this.Call<'T when 'T : not struct>(connection : DbConnection, procedure) = 
            this.Call<'T>(connection, procedure)
        member this.CreateConnection() = this.CreateConnection()
        member this.Queryable<'T when 'T : not struct>(connection : DbConnection) = this.Queryable<'T>(connection)
        member this.QueryableDelete<'T when 'T : not struct>(connection : DbConnection, query) = 
            this.QueryableDelete<'T>(connection, query)

type PlainDb(config : IDbConfig) = 
    do Guard.argNotNull (config, "config")
    let db = new DbImpl(config)
    
    let createExprCtxt (condition : IDictionary) = 
        let exprCtxt = Dictionary<string, obj * Type>(condition.Count)
        condition.Keys
        |> Seq.cast<obj>
        |> Seq.iter (fun key -> 
               let key = 
                   if key <> null then key.ToString()
                   else null
               
               let value = condition.[key]
               
               let typ = 
                   if value = null || value = Convert.DBNull then typeof<obj>
                   else value.GetType()
               exprCtxt.[key] <- (value, typ))
        exprCtxt
    
    let convertToHashtable (dict : IDictionary) = 
        Hashtable(dict, StringComparer.InvariantCultureIgnoreCase) :> IDictionary
    abstract DbConfig : IDbConfig
    override this.DbConfig = config
    abstract Query : sql:string -> IDictionary IList
    
    override this.Query(sql : string) = 
        Guard.argNotNull (sql, "sql")
        let seq = db.QueryOnDemand<dynamic> sql Map.empty |> Seq.map convertToHashtable
        ResizeArray(seq) :> IList<IDictionary>
    
    abstract Query : sql:string * condition:IDictionary -> IDictionary IList
    
    override this.Query(sql : string, condition : IDictionary) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = createExprCtxt condition
        let seq = db.QueryOnDemand<dynamic> sql exprCtxt |> Seq.map convertToHashtable
        ResizeArray(seq) :> IList<IDictionary>
    
    abstract QueryOnDemand : sql:string -> IDictionary seq
    
    override this.QueryOnDemand(sql : string) = 
        Guard.argNotNull (sql, "sql")
        db.QueryOnDemand<dynamic> sql Map.empty |> Seq.map convertToHashtable
    
    abstract QueryOnDemand : sql:string * condition:IDictionary -> IDictionary seq
    
    override this.QueryOnDemand(sql : string, condition : IDictionary) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = createExprCtxt condition
        db.QueryOnDemand<dynamic> sql exprCtxt |> Seq.map convertToHashtable
    
    abstract Paginate : sql:string * offset:int64 * limit:int64 -> IDictionary IList
    
    override this.Paginate(sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (sql, "sql")
        let seq = db.PaginateOnDemand<dynamic> sql Map.empty (offset, limit) |> Seq.map convertToHashtable
        ResizeArray(seq) :> IList<IDictionary>
    
    abstract Paginate : sql:string * offset:int64 * limit:int64 * condition:IDictionary -> IDictionary IList
    
    override this.Paginate(sql : string, offset : int64, limit : int64, condition : IDictionary) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = createExprCtxt condition
        let seq = db.PaginateOnDemand<dynamic> sql exprCtxt (offset, limit) |> Seq.map convertToHashtable
        ResizeArray(seq) :> IList<IDictionary>
    
    abstract PaginateOnDemand : sql:string * offset:int64 * limit:int64 -> IDictionary seq
    
    override this.PaginateOnDemand(sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (sql, "sql")
        db.PaginateOnDemand<dynamic> sql Map.empty (offset, limit) |> Seq.map convertToHashtable
    
    abstract PaginateOnDemand : sql:string * offset:int64 * limit:int64 * condition:IDictionary -> IDictionary seq
    
    override this.PaginateOnDemand(sql : string, offset : int64, limit : int64, condition : IDictionary) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = createExprCtxt condition
        db.PaginateOnDemand<dynamic> sql exprCtxt (offset, limit) |> Seq.map convertToHashtable
    
    abstract PaginateAndCount : sql:string * offset:int64 * limit:int64 -> IDictionary IList * int64
    
    override this.PaginateAndCount(sql : string, offset : int64, limit : int64) = 
        Guard.argNotNull (sql, "sql")
        let list, count = db.PaginateAndCount<dynamic> sql Map.empty (offset, limit)
        let seq = list |> Seq.map convertToHashtable
        ResizeArray(seq) :> IList<IDictionary>, count
    
    abstract PaginateAndCount : sql:string * offset:int64 * limit:int64 * condition:IDictionary
     -> IDictionary IList * int64
    
    override this.PaginateAndCount(sql : string, offset : int64, limit : int64, condition : IDictionary) = 
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let exprCtxt = createExprCtxt condition
        let list, count = db.PaginateAndCount<dynamic> sql exprCtxt (offset, limit)
        let seq = list |> Seq.map convertToHashtable
        ResizeArray(seq) :> IList<IDictionary>, count
    
    abstract Execute : sql:string -> int
    
    override this.Execute(sql : string) = 
        Guard.argNotNull (sql, "sql")
        db.Execute sql Map.empty
    
    abstract Execute : sql:string * condition:IDictionary -> int
    override this.Execute(sql : string, condition : IDictionary) = 
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
        db.QueryOnDemand<'T> sql (Conversion.fromListToExprCtxt condition) |> Seq.toList
    
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
        db.PaginateOnDemand<'T> sql (Conversion.fromListToExprCtxt condition) (offset, limit) |> Seq.toList
    
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
    
    let insertOrUpdate<'T when 'T : not struct> config entity = 
        Guard.argNotNull (config, "config")
        Guard.argNotNull (entity, "entity")
        let db = DbImpl config
        db.InsertOrUpdate<'T> entity
    
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
    
    let queryable<'T when 'T : not struct> config = 
        Guard.argNotNull (config, "config")
        let db = DbImpl config
        db.Queryable<'T>()
    
    let queryableDelete<'T when 'T : not struct> config query = 
        Guard.argNotNull (config, "config")
        let db = DbImpl config
        db.QueryableDelete<'T>(query)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LocalDb = 
    let query<'T> config connection sql condition = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (config, "config")
        Guard.argNotNull (sql, "sql")
        Guard.argNotNull (condition, "condition")
        let db = LocalDbImpl(config, connection)
        db.QueryOnDemand<'T> sql (Conversion.fromListToExprCtxt condition) |> Seq.toList
    
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
        db.PaginateOnDemand<'T> sql (Conversion.fromListToExprCtxt condition) (offset, limit) |> Seq.toList
    
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
    
    let insertOrUpdate<'T when 'T : not struct> config connection entity = 
        Guard.argNotNull (connection, "connection")
        Guard.argNotNull (config, "config")
        Guard.argNotNull (entity, "entity")
        let db = LocalDbImpl(config, connection)
        db.InsertOrUpdate<'T> entity
    
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
    
    let createConnection (config : IDbConfig) = 
        let connection = config.DbProviderFactory.CreateConnection()
        connection.ConnectionString <- config.ConnectionString
        connection
    
    let queryable<'T when 'T : not struct> config connection : System.Linq.IQueryable<'T> = 
        Guard.argNotNull (config, "config")
        Guard.argNotNull (connection, "connection")
        let db = LocalDbImpl(config, connection)
        db.Queryable<'T>()
    
    let queryableDelete<'T when 'T : not struct> config connection (query : System.Linq.IQueryable<'T>) : unit = 
        Guard.argNotNull (config, "config")
        Guard.argNotNull (connection, "connection")
        let db = LocalDbImpl(config, connection)
        db.QueryableDelete(query)

[<AutoOpen>]
module DynamicOperations = 
    let (?) (dynamic : dynamic) (propName : string) : 'a = 
        let value = dynamic.[propName]
        let destType = typeof<'a>
        try 
            dynamic.Dialect.ConvertFromDbToClr(value, destType, null, null) :?> 'a
        with exn -> 
            let typ = 
                if value = null then typeof<obj>
                else value.GetType()
            raise <| Soma.Core.DbException(SR.SOMA4027(typ.FullName, propName, destType.FullName), exn)
    
    let inline (?<-) (dynamic : dynamic) (propName : string) (value : 'a) = dynamic.[propName] <- value
    
    [<CompiledName("Dynamic")>]
    let dynamic dialect = 
        Guard.argNotNull (dialect, "dialect")
        CaseInsensitiveDynamicObject(dialect) :> dynamic

[<AutoOpen>]
module UtilityOperations = 
    let inline (@=) (name : string) (value : 'a) = name, box value, typeof<'a>
