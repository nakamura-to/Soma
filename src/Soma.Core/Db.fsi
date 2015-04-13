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
open System.ComponentModel
open System.Data
open System.Data.Common
open System.Dynamic
open System.Collections
open System.Collections.Generic
open System.Runtime.InteropServices

/// <summary>Represents a base class of the Soma.Core.IDbConfig.</summary>
[<AbstractClass>]
type DbConfigBase =
  interface IDbConfig

  /// <summary>Initializes a DbConfigBase instance.</summary>
  /// <param name="invariant">The invariant name of a provider.</param>
  new : invariant:string -> DbConfigBase

  /// <summary>Gets the cache enabled SQL Parser.</summary>
  member CacheSqlParser : Func<string, SqlAst.Statement>

  /// <summary>Gets the cache unenabled SQL Parser.</summary>
  member NoCacheSqlParser : Func<string, SqlAst.Statement>

  /// <summary>Gets the cache enabled expression Parser.</summary>
  member CacheExpressionParser : Func<string, ExpressionAst.Expression>

  /// <summary>Gets the cache unenabled expression Parser.</summary>
  member NoCacheExpressionParser : Func<string, ExpressionAst.Expression>

  /// <summary>Gets the console Logger.</summary>
  member ConsoleLogger : Action<PreparedStatement>

  /// <summary>Gets the silent Logger.</summary>
  member SilentLogger : Action<PreparedStatement>

  /// <summary>Gets the invariant name of a provider.</summary>
  member Invariant : string

  /// <summary>Gets the db provider factory.</summary>
  member DbProviderFactory : DbProviderFactory

  /// <summary>Gets the string used to open the connection.</summary>
  abstract ConnectionString : string

  /// <summary>Gets the SQL dialect.</summary>
  abstract Dialect : IDialect

  /// <summary>Gets the SQL Parser.</summary>
  /// <remarks>Default implementation is CacheSqlParser.</remarks>
  abstract SqlParser : Func<string, SqlAst.Statement>
  default SqlParser : Func<string, SqlAst.Statement>

  /// <summary>Gets the expression Parser.</summary>
  /// <remarks>Default implementation is CacheExpressionParser.</remarks>
  abstract ExpressionParser : Func<string, ExpressionAst.Expression>
  default ExpressionParser : Func<string, ExpressionAst.Expression>

  /// <summary>Gets the SQL Logger.</summary>
  /// <remarks>Default implementation is ConsoleLogger.</remarks>
  abstract Logger : Action<PreparedStatement>
  default Logger : Action<PreparedStatement>

  /// <summary>Gets the Connection Observer.</summary>
  abstract ConnectionObserver : IConnectionObserver
  default ConnectionObserver : IConnectionObserver

  /// <summary>Gets the Command Observer.</summary>
  abstract CommandObserver : ICommandObserver
  default CommandObserver : ICommandObserver


/// <summary>Represents a database configuration of Microsoft SQL Server 2008.</summary>
[<AbstractClass>]
type MsSqlConfig =
  inherit DbConfigBase

  /// <summary>Initializes a MsSqlConfig instance.</summary>
  new : unit -> MsSqlConfig

  /// <summary>Gets the SQL dialect.</summary>
  override Dialect : IDialect

/// <summary>Represents a database configuration of Microsoft SQL Server Compact 4.0.</summary>
[<AbstractClass>]
type MsSqlCeConfig =
  inherit DbConfigBase

  /// <summary>Initializes a MsSqlCeConfig instance.</summary>
  new : unit -> MsSqlCeConfig

  /// <summary>Gets the SQL dialect.</summary>
  override Dialect : IDialect

/// <summary>Represents a database configuration of MySQL 5.x.</summary>
[<AbstractClass>]
type MySqlConfig =
  inherit DbConfigBase

  /// <summary>Initializes a MySqlConfig instance.</summary>
  new : unit -> MySqlConfig

  /// <summary>Gets the SQL dialect.</summary>
  override Dialect : IDialect

/// <summary>Represents a database configuration of Oracle Database 11g.</summary>
[<AbstractClass>]
type OracleConfig =
  inherit DbConfigBase

  /// <summary>Initializes a OracleConfig instance.</summary>
  new : unit -> OracleConfig

  /// <summary>Gets the SQL dialect.</summary>
  override Dialect : IDialect

/// <summary>Represents a database configuration of SQLite.</summary>
[<AbstractClass>]
type SQLiteConfig =
  inherit DbConfigBase

  /// <summary>Initializes a SQLiteConfig instance.</summary>
  new : unit -> SQLiteConfig

  /// <summary>Gets the SQL dialect.</summary>
  override Dialect : IDialect

/// <summary>Represents a plain database configuration.</summary>
type PlainConfig =
  inherit DbConfigBase

  /// <summary>Initializes a PlainConfig instance.</summary>
  /// <param name="invariant">The invariant.</param>
  /// <param name="connectionString">The connection string.</param>
  /// <param name="dialect">The SQL dialect.</param>
  new : invariant:string * connectionString:string * dialect:IDialect -> PlainConfig

  /// <summary>Gets the connection string.</summary>
  override ConnectionString : string

  /// <summary>Gets the SQL dialect.</summary>
  override Dialect : IDialect

  /// <summary>Gets the SQL logger.</summary>
  override Logger : Action<PreparedStatement>

  /// <summary>Sets the SQL logger.</summary>
  member SetLogger : Action<PreparedStatement> -> unit

/// <summary>Represents operations on the Database.</summary>
/// <exception cref="System.ArgumentNullException">Thrown when any arguments are null.</exception>
[<Interface>]
type IDb =
  /// <summary>Gets the databse configuration.</summary>
  abstract DbConfig : IDbConfig

  /// <summary>Queries the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result list.</returns>
  abstract Query<'T> : sql:string -> 'T IList

  /// <summary>Queries the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The query condition.</param>
  /// <returns>The query result list.</returns>
  abstract Query<'T> : sql:string * condition:obj -> 'T IList

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand<'T> : sql:string -> 'T seq

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand<'T> : sql:string * condition:obj -> 'T seq

  /// <summary>Paginates the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate<'T> : sql:string * offset:int64 * limit:int64 -> 'T IList

  /// <summary>Paginates the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand<'T> : sql:string * offset:int64 * limit:int64 -> 'T seq

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T seq

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount<'T> : sql:string * offset:int64 * limit:int64 -> 'T IList * int64

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList * int64

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : sql:string -> int

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : sql:string * condition:obj -> int

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The result.</returns>
  abstract ExecuteReader<'T> : handler:Func<DbDataReader, 'T> * sql:string -> 'T

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The result.</returns>
  abstract ExecuteReader<'T> : handler:Func<DbDataReader, 'T> * sql:string * condition:obj -> 'T

  /// <summary>Finds the entity.</summary>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : id:obj -> 'T

  /// <summary>Try to find the entity.</summary>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <returns>The found entity or null.</returns>
  abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : id:obj -> 'T
  
  /// <summary>Finds the entity with the version.</summary>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : id:obj * version:obj -> 'T

  /// <summary>Try to find the entity with the version.</summary>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity or null.</returns>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>  : id:obj * version:obj -> 'T

  /// <summary>Inserts the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <returns>The inserted entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  abstract Insert<'T when 'T : not struct> : entity:'T -> unit

  /// <summary>Inserts the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <returns>The inserted entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  abstract Insert<'T when 'T : not struct> : entity:'T * opt:InsertOpt -> unit

  /// <summary>Updates the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  abstract Update<'T when 'T : not struct> : entity:'T -> unit

  /// <summary>Updates the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  abstract Update<'T when 'T : not struct> : entity:'T * opt:UpdateOpt -> unit

  /// <summary>Deletes the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract Delete<'T when 'T : not struct> : entity:'T -> unit

  /// <summary>Deletes the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract Delete<'T when 'T : not struct> : entity:'T * opt:DeleteOpt -> unit

  /// <summary>Calls the stored procedure.</summary>
  /// <param name="procedure">The stored procedure.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Call<'T when 'T : not struct> : procedure:'T -> unit

  /// <summary>Creates an IQueryable on 'T</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <returns>The IQueryable for 'T</returns>
  abstract Queryable<'T when 'T : not struct> : unit -> System.Linq.IQueryable<'T>
    
  /// <summary>Deletes all values returned by query.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="query">The query which values will be deleted.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract QueryableDelete<'T when 'T : not struct> : query:System.Linq.IQueryable<'T> -> unit

/// <summary>Implements <c>Soma.Core.IDb</c>.</summary>
/// <exception cref="System.ArgumentNullException">Thrown when any arguments are null.</exception>
type Db =
  interface IDb
  /// <summary>Initializes a Db instance.</summary>
  /// <param name="config">The database configuration.</param>
  new : config:IDbConfig -> Db

  /// <summary>Gets the databse configuration.</summary>
  abstract DbConfig : IDbConfig

  /// <summary>Queries the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result list.</returns>
  abstract Query<'T> : sql:string -> 'T IList

  /// <summary>Queries the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The query condition.</param>
  /// <returns>The query result list.</returns>
  abstract Query<'T> : sql:string * condition:obj -> 'T IList

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand<'T> : sql:string -> 'T seq

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand<'T> : sql:string * condition:obj -> 'T seq

  /// <summary>Paginates the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate<'T> : sql:string * offset:int64 * limit:int64 -> 'T IList

  /// <summary>Paginates the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand<'T> : sql:string * offset:int64 * limit:int64 -> 'T seq

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T seq

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount<'T> : sql:string * offset:int64 * limit:int64 -> 'T IList * int64

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount<'T> : sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList * int64

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : sql:string -> int

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : sql:string * condition:obj -> int

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The result.</returns>
  abstract ExecuteReader<'T> : handler:Func<DbDataReader, 'T> * sql:string -> 'T

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The result.</returns>
  abstract ExecuteReader<'T> : handler:Func<DbDataReader, 'T> * sql:string * condition:obj -> 'T

  /// <summary>Finds the entity.</summary>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : id:obj -> 'T

  /// <summary>Try to find the entity.</summary>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <returns>The found entity or null.</returns>
  abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : id:obj -> 'T
  
  /// <summary>Finds the entity with the version.</summary>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : id:obj * version:obj -> 'T

  /// <summary>Try to find the entity with the version.</summary>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity or null.</returns>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>  : id:obj * version:obj -> 'T

  /// <summary>Inserts the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  abstract Insert<'T when 'T : not struct> : entity:'T -> unit

  /// <summary>Inserts the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  abstract Insert<'T when 'T : not struct> : entity:'T * opt:InsertOpt -> unit

  /// <summary>Updates the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  abstract Update<'T when 'T : not struct> : entity:'T -> unit

  /// <summary>Updates the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  abstract Update<'T when 'T : not struct> : entity:'T * opt:UpdateOpt -> unit

  /// <summary>Deletes the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract Delete<'T when 'T : not struct> : entity:'T -> unit

  /// <summary>Deletes the entity.</summary>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract Delete<'T when 'T : not struct> : entity:'T * opt:DeleteOpt -> unit

  /// <summary>Calls the stored procedure.</summary>
  /// <param name="procedure">The stored procedure.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Call<'T when 'T : not struct> : procedure:'T -> unit

  /// <summary>Creates an IQueryable on 'T</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <returns>The IQueryable for 'T</returns>
  abstract Queryable<'T when 'T : not struct> : unit -> System.Linq.IQueryable<'T>
    
  /// <summary>Deletes all values returned by query.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="query">The query which values will be deleted.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract QueryableDelete<'T when 'T : not struct> : query:System.Linq.IQueryable<'T> -> unit

/// <summary>Represents operations on the local Database.</summary>
/// <exception cref="System.ArgumentNullException">Thrown when any arguments are null.</exception>
[<Interface>]
type ILocalDb =

  /// <summary>Gets the databse configuration.</summary>
  abstract DbConfig : IDbConfig

  /// <summary>Queries the rows.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result list.</returns>
  abstract Query<'T> : connection:DbConnection * sql:string -> 'T IList

  /// <summary>Queries the rows.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The query condition.</param>
  /// <returns>The query result list.</returns>
  abstract Query<'T> : connection:DbConnection * sql:string * condition:obj -> 'T IList

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand<'T> : connection:DbConnection * sql:string -> 'T seq

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand<'T> : connection:DbConnection * sql:string * condition:obj -> 'T seq

  /// <summary>Paginates the rows.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T IList

  /// <summary>Paginates the rows.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T seq

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T seq

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T IList * int64

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList * int64

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : connection:DbConnection * sql:string -> int

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : connection:DbConnection * sql:string * condition:obj -> int

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The result.</returns>
  abstract ExecuteReader<'T> : connection:DbConnection * handler:Func<DbDataReader, 'T> * sql:string -> 'T

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The result.</returns>
  abstract ExecuteReader<'T> : connection:DbConnection * handler:Func<DbDataReader, 'T> * sql:string * condition:obj -> 'T

  /// <summary>Finds the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : connection:DbConnection * id:obj -> 'T

  /// <summary>Try to find the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <returns>The found entity or null.</returns>
  abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : connection:DbConnection * id:obj -> 'T
  
  /// <summary>Finds the entity with the version.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : connection:DbConnection * id:obj * version:obj -> 'T

  /// <summary>Try to find the entity with the version.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity or null.</returns>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>  : connection:DbConnection * id:obj * version:obj -> 'T

  /// <summary>Inserts the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  abstract Insert<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit

  /// <summary>Inserts the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  abstract Insert<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:InsertOpt -> unit

  /// <summary>Updates the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  abstract Update<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit

  /// <summary>Updates the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  abstract Update<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:UpdateOpt -> unit

  /// <summary>Deletes the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract Delete<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit

  /// <summary>Deletes the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract Delete<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:DeleteOpt -> unit

  /// <summary>Calls the stored procedure.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="procedure">The stored procedure.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Call<'T when 'T : not struct> : connection:DbConnection * procedure:'T -> unit

  /// <summary>Creates an IQueryable on 'T</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <returns>The IQueryable for 'T</returns>
  abstract Queryable<'T when 'T : not struct> : connection:DbConnection -> System.Linq.IQueryable<'T>
    
  /// <summary>Deletes all values returned by query.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="query">The query which values will be deleted.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract QueryableDelete<'T when 'T : not struct> : connection:DbConnection * query:System.Linq.IQueryable<'T> -> unit

  /// <summary>Creates the connection.</summary>
  /// <returns>The connection.</returns>
  abstract CreateConnection : unit -> DbConnection

/// <summary>Implements <c>Soma.Core.ILocalDb</c>.</summary>
/// <exception cref="System.ArgumentNullException">Thrown when any arguments are null.</exception>
type LocalDb =
  interface ILocalDb

  /// <summary>Initializes a Db instance.</summary>
  /// <param name="config">The database configuration.</param>
  new : config:IDbConfig -> LocalDb

  /// <summary>Gets the databse configuration.</summary>
  abstract DbConfig : IDbConfig

  /// <summary>Queries the rows.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result list.</returns>
  abstract Query<'T> : connection:DbConnection * sql:string -> 'T IList

  /// <summary>Queries the rows.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The query condition.</param>
  /// <returns>The query result list.</returns>
  abstract Query<'T> : connection:DbConnection * sql:string * condition:obj -> 'T IList

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand<'T> : connection:DbConnection * sql:string -> 'T seq

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand<'T> : connection:DbConnection * sql:string * condition:obj -> 'T seq

  /// <summary>Paginates the rows.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T IList

  /// <summary>Paginates the rows.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T seq

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T seq

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 -> 'T IList * int64

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount<'T> : connection:DbConnection * sql:string * offset:int64 * limit:int64 * condition:obj -> 'T IList * int64

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : connection:DbConnection * sql:string -> int

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : connection:DbConnection * sql:string * condition:obj -> int

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <returns>The result.</returns>
  abstract ExecuteReader<'T> : connection:DbConnection * handler:Func<DbDataReader, 'T> * sql:string -> 'T

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The result.</returns>
  abstract ExecuteReader<'T> : connection:DbConnection * handler:Func<DbDataReader, 'T> * sql:string * condition:obj -> 'T

  /// <summary>Finds the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  abstract Find<'T when 'T : not struct and 'T : (new : unit -> 'T)> : connection:DbConnection * id:obj -> 'T

  /// <summary>Try to find the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <returns>The found entity or null.</returns>
  abstract TryFind<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null> : connection:DbConnection * id:obj -> 'T
  
  /// <summary>Finds the entity with the version.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  abstract FindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T)> : connection:DbConnection * id:obj * version:obj -> 'T

  /// <summary>Try to find the entity with the version.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The primary key or the list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity or null.</returns>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  abstract TryFindWithVersion<'T when 'T : not struct and 'T : (new : unit -> 'T) and 'T : null>  : connection:DbConnection * id:obj * version:obj -> 'T

  /// <summary>Inserts the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  abstract Insert<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit

  /// <summary>Inserts the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  abstract Insert<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:InsertOpt -> unit

  /// <summary>Updates the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  abstract Update<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit

  /// <summary>Updates the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  abstract Update<'T when 'T : not struct> : connection:DbConnection * entity:'T * opt:UpdateOpt -> unit

  /// <summary>Deletes the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract Delete<'T when 'T : not struct> : connection:DbConnection * entity:'T -> unit

  /// <summary>Deletes the entity.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract Delete<'T when 'T : not struct> : connection:DbConnection * entity:'T  * opt:DeleteOpt -> unit

  /// <summary>Calls the stored procedure.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="procedure">The stored procedure.</param>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Call<'T when 'T : not struct> : connection:DbConnection * procedure:'T -> unit

  /// <summary>Creates an IQueryable on 'T</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <returns>The IQueryable for 'T</returns>
  abstract Queryable<'T when 'T : not struct> : connection:DbConnection -> System.Linq.IQueryable<'T>
    
  /// <summary>Deletes all values returned by query.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="query">The query which values will be deleted.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  abstract QueryableDelete<'T when 'T : not struct> : connection:DbConnection * query:System.Linq.IQueryable<'T> -> unit

  /// <summary>Creates the connection.</summary>
  /// <returns>The connection.</returns>
  abstract CreateConnection : unit -> DbConnection

/// <summary>Represents plain Database.</summary>
/// <exception cref="System.ArgumentNullException">Thrown when any arguments are null.</exception>
type PlainDb =
  /// <summary>Initializes a Db instance.</summary>
  /// <param name="config">The database configuration.</param>
  new : config:IDbConfig -> PlainDb

  /// <summary>Gets the databse configuration.</summary>
  abstract DbConfig : IDbConfig

  /// <summary>Queries the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result list.</returns>
  abstract Query : sql:string -> IDictionary IList

  /// <summary>Queries the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list.</returns>
  abstract Query : sql:string  * condition:IDictionary -> IDictionary IList

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand : sql:string -> IDictionary seq

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract QueryOnDemand : sql:string * condition:IDictionary -> IDictionary seq

  /// <summary>Paginates the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate : sql:string * offset:int64 * limit:int64 -> IDictionary IList

  /// <summary>Paginates the rows.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list.</returns>
  abstract Paginate : sql:string * offset:int64 * limit:int64 * condition:IDictionary -> IDictionary IList

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand : sql:string * offset:int64 * limit:int64 -> IDictionary seq

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  abstract PaginateOnDemand : sql:string * offset:int64 * limit:int64 * condition:IDictionary -> IDictionary seq

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount : sql:string * offset:int64 * limit:int64 -> IDictionary IList * int64

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="offset">The offset. Nagative value means zero.</param>
  /// <param name="limit">The max row count. Negative value means infinite.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result list and the count.</returns>
  abstract PaginateAndCount : sql:string * offset:int64 * limit:int64 * condition:IDictionary -> IDictionary IList * int64

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : sql:string -> int

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  abstract Execute : sql:string * condition:IDictionary -> int

/// <summary>Operations on the Database.</summary>
/// <exception cref="System.ArgumentNullException">Thrown when any arguments are null.</exception>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Db =

  /// <summary>Queries the rows.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The query condition.</param>
  /// <returns>The query result list.</returns>
  val query<'T> : config:IDbConfig -> sql:string -> condition:(string * obj * Type) list -> 'T list

  /// <summary>Queries the rows on demand.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  val queryOnDemand<'T> : config:IDbConfig -> sql:string -> condition:(string * obj * Type) list -> 'T seq

  /// <summary>Paginates the rows.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <param name="offset">The offset.</param>
  /// <param name="limit">The limit.</param>
  /// <returns>The query result list.</returns>
  val paginate<'T> : config:IDbConfig -> sql:string -> condition:(string * obj * Type) list -> offset:int64 * limit:int64 -> 'T list

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <param name="offset">The offset.</param>
  /// <param name="limit">The limit.</param>
  /// <returns>The query result sequence.</returns>
  val paginateOnDemand<'T> : config:IDbConfig -> sql:string -> condition:(string * obj * Type) list -> offset:int64 * limit:int64-> 'T seq

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <param name="offset">The offset.</param>
  /// <param name="limit">The limit.</param>
  /// <returns>The query result list and the count.</returns>
  val paginateAndCount<'T> : config:IDbConfig -> sql:string -> condition:(string * obj * Type) list -> offset:int64 * limit:int64 -> 'T list * int64

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  val execute : config:IDbConfig -> sql:string -> condition:(string * obj * Type) list -> int  

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The result.</returns>
  val executeReader<'T> : config:IDbConfig -> handler:(DbDataReader -> 'T) -> sql:string -> condition:(string * obj * Type) list -> 'T

  /// <summary>Finds the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="id">The list of primary keys.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  val find<'T when 'T : not struct> : config:IDbConfig -> id:obj list -> 'T

  /// <summary>Try to find the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="id">The list of primary keys.</param>
  /// <returns>The found entity or <c>None</c>.</returns>
  val tryFind<'T when 'T : not struct> : config:IDbConfig -> id:obj list -> 'T option

  /// <summary>Finds the entity with the version.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="id">The list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  val findWithVersion<'T when 'T : not struct> : config:IDbConfig -> id:obj list -> version:obj -> 'T

  /// <summary>Try to find the entity with the version.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="id">The list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity or <c>None</c>.</returns>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  val tryFindWithVersion<'T when 'T : not struct> : config:IDbConfig -> id:obj list -> version:obj -> 'T option

  /// <summary>Inserts the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="entity">The entity.</param>
  /// <returns>The inserted entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  val insert<'T when 'T : not struct> : config:IDbConfig -> entity:'T -> 'T

  /// <summary>Inserts the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <returns>The inserted entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  val insertWithOpt<'T when 'T : not struct> : config:IDbConfig -> entity:'T -> opt:InsertOpt -> 'T

  /// <summary>Updates the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="entity">The entity.</param>
  /// <returns>The updated entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  val update<'T when 'T : not struct> : config:IDbConfig -> entity:'T -> 'T

  /// <summary>Updates the entity with options.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <returns>The updated entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  val updateWithOpt<'T when 'T : not struct> : config:IDbConfig -> entity:'T -> opt:UpdateOpt -> 'T

  /// <summary>Deletes the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  val delete<'T when 'T : not struct> : config:IDbConfig -> entity:'T -> unit

  /// <summary>Deletes the entity with options.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  val deleteWithOpt<'T when 'T : not struct> : config:IDbConfig -> entity:'T -> opt:DeleteOpt -> unit

  /// <summary>Calls the stored procedure.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="procedure">The stored procedure.</param>
  /// <returns>The called stored procedure.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  val call<'T when 'T : not struct> : config:IDbConfig -> procedure:'T -> 'T
  
  /// <summary>Creates an IQueryable on 'T</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <returns>The IQueryable for 'T</returns>
  val queryable<'T when 'T : not struct> : config:IDbConfig -> System.Linq.IQueryable<'T>
    
  /// <summary>Deletes all values returned by query.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="query">The query which values will be deleted.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  val queryableDelete<'T when 'T : not struct> : config:IDbConfig -> query:System.Linq.IQueryable<'T> -> unit

/// <summary>Operations on the local Database.</summary>
/// <exception cref="System.ArgumentNullException">Thrown when any arguments are null.</exception>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LocalDb =
  /// <summary>Queries the rows.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The query condition.</param>
  /// <returns>The query result list.</returns>
  val query<'T> : config:IDbConfig -> connection:DbConnection -> sql:string -> condition:(string * obj * Type) list -> 'T list
  
  /// <summary>Queries the rows on demand.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The query result sequence.</returns>
  val queryOnDemand<'T> : config:IDbConfig -> connection:DbConnection -> sql:string -> condition:(string * obj * Type) list -> 'T seq
  
  /// <summary>Paginates the rows.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <param name="offset">The offset.</param>
  /// <param name="limit">The limit.</param>
  /// <returns>The query result list.</returns>
  val paginate<'T> : config:IDbConfig -> connection:DbConnection -> sql:string -> condition:(string * obj * Type) list -> offset:int64 * limit:int64 -> 'T list

  /// <summary>Paginates the rows on demand.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <param name="offset">The offset.</param>
  /// <param name="limit">The limit.</param>
  /// <returns>The query result sequence.</returns>
  val paginateOnDemand<'T> : config:IDbConfig -> connection:DbConnection -> sql:string -> condition:(string * obj * Type) list -> offset:int64 * limit:int64-> 'T seq

  /// <summary>Paginates the rows, and counts rows without the offset and the limit.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <param name="offset">The offset.</param>
  /// <param name="limit">The limit.</param>
  /// <returns>The query result list and the count.</returns>
  val paginateAndCount<'T> : config:IDbConfig -> connection:DbConnection -> sql:string -> condition:(string * obj * Type) list -> offset:int64 * limit:int64 -> 'T list * int64

  /// <summary>Executes the arbitrary SQL.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The affected rows.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  val execute : config:IDbConfig -> connection:DbConnection -> sql:string -> condition:(string * obj * Type) list ->  int

  /// <summary>Executes the SQL and handles the reader.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="handler">The reader handler.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The result.</returns>
  val executeReader<'T> : config:IDbConfig -> connection:DbConnection -> handler:(DbDataReader -> 'T) -> sql:string -> condition:(string * obj * Type) list -> 'T

  /// <summary>Finds the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The list of primary keys.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  val find<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> id:obj list -> 'T

  /// <summary>Try to find the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The list of primary keys.</param>
  /// <returns>The found entity or <c>None</c>.</returns>
  val tryFind<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> id:obj list -> 'T option

  /// <summary>Finds the entity with the version.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity.</returns>
  /// <exception cref="Soma.Core.EntityNotFoundException">Thrown when the entity is not found.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  val findWithVersion<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> id:obj list -> version:obj -> 'T

  /// <summary>Try to find the entity with the version.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="id">The list of primary keys.</param>
  /// <param name="version">The expected version.</param>
  /// <returns>The found entity or <c>None</c>.</returns>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the found entity version is different from the expected version.</exception>
  val tryFindWithVersion<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> id:obj list -> version:obj -> 'T option

  /// <summary>Inserts the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <returns>The inserted entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  val insert<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> entity:'T -> 'T

  /// <summary>Inserts the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <returns>The inserted entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoInsertablePropertyException">Thrown when there is no insertable property.</exception>
  val insertWithOpt<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> entity:'T -> opt:InsertOpt -> 'T

  /// <summary>Updates the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <returns>The updated entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  val update<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> entity:'T -> 'T

  /// <summary>Updates the entity without the version.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <returns>The updated entity.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  /// <exception cref="Soma.Core.NoUpdatablePropertyException">Thrown when there is no updatable property.</exception>
  val updateWithOpt<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> entity:'T -> opt:UpdateOpt -> 'T

  /// <summary>Deletes the entity.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  val delete<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> 'T -> unit

  /// <summary>Deletes the entity without the version.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="entity">The entity.</param>
  /// <param name="opt">The options.</param>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when and there is no affected row.</exception>
  val deleteWithOpt<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> entity:'T -> opt:DeleteOpt -> unit

  /// <summary>Calls the stored procedure.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="procedure">The stored procedure.</param>
  /// <returns>The called stored procedure.</returns>
  /// <exception cref="Soma.Core.UniqueConstraintException">Thrown when a unique constraint violation is occurred.</exception>
  val call<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> procedure:'T -> 'T

  /// <summary>Creates the connection.</summary>
  /// <param name="config">The database configuration.</param>
  /// <returns>The connection.</returns>
  val createConnection : config:IDbConfig -> DbConnection

  /// <summary>Creates an IQueryable on 'T</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <returns>The IQueryable for 'T</returns>
  val queryable<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> System.Linq.IQueryable<'T>
    
  /// <summary>Deletes all values returned by query.</summary>
  /// <param name="config">The database configuration.</param>
  /// <param name="connection">The connection.</param>
  /// <param name="query">The query which values will be deleted.</param>
  /// <exception cref="Soma.Core.OptimisticLockException">Thrown when the entity version is different from the expected version.</exception>
  /// <exception cref="Soma.Core.NoAffectedRowException">Thrown when there is no affected row.</exception>
  val queryableDelete<'T when 'T : not struct> : config:IDbConfig -> connection:DbConnection -> query:System.Linq.IQueryable<'T> -> unit
    
/// <summary>The dynamic Operations.</summary>
[<AutoOpen>]
module DynamicOperations =

  /// <summary>Gets the property value.</summary>
  /// <param name="dynamic">The dynamic object.</param>
  /// <param name="propName">The property name.</param>
  /// <returns>The property value.</returns>
  val (?): dynamic:dynamic -> propName:string -> 'a

  /// <summary>Sets the property value.</summary>
  /// <param name="dynamic">The dynamic object.</param>
  /// <param name="propName">The property name.</param>
  /// <param name="value">The property value.</param>
  val inline (?<-) : dynamic:dynamic -> propName:string -> value:'a -> unit

  /// <summary>Creates dynamic object with the SQL dialect.</summary>
  /// <param name="dialect">The SQL dialect.</param>
  /// <returns>The dynamic object.</returns>
  [<CompiledName("Dynamic")>]
  val dynamic : dialect:IDialect -> dynamic

/// <summary>The utility Operations.</summary>
[<AutoOpen>]
module UtilityOperations =

  /// <summary>Makes name/value pair.</summary>
  /// <param name="name">The name.</param>
  /// <param name="value">The value.</param>
  /// <returns>The pair.</returns>
  val inline (@=) : name:string -> value:'a -> string * obj * Type

module internal Conversion =
  
  val toIdList : obj -> obj list

  val toExprCtxt : obj -> IDictionary<string, obj * Type>