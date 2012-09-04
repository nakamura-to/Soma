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
open System.Collections.Generic
open System.Data
open System.Data.Common
open System.Reflection

/// <summary>Indicates that a class is mapped to a table.</summary>
[<AttributeUsage(AttributeTargets.Class)>]
type TableAttribute =
  inherit Attribute
  /// <summary>Initializes a TableAttribute instance.</summary>
  new : unit -> TableAttribute

  /// <summary>Gets and Sets the catalog name.</summary>
  member Catalog : string with get, set

  /// <summary>Gets and Sets the schema name.</summary>
  member Schema : string with get, set

  /// <summary>Gets and Sets the table name.</summary>
  member Name : string with get, set

  /// <summary>Gets and Sets whether names are enclosed or not.</summary>
  member IsEnclosed : bool with get, set

/// <summary>Indicates that a property is mapped to a column.</summary>
[<AttributeUsage(AttributeTargets.Property)>]
type ColumnAttribute = 
  inherit Attribute

  /// <summary>Initializes a ColumnAttribute instance.</summary>
  new : unit -> ColumnAttribute

  /// <summary>Gets and Sets the column name.</summary>
  member Name : string with get, set

  /// <summary>Gets and Sets whether the collumn is insertable or not.</summary>
  member Insertable : bool with get, set

  /// <summary>Gets and Sets whether the collumn is updatable or not.</summary>
  member Updatable : bool with get, set

  /// <summary>Gets and Sets whether names are enclosed or not.</summary>
  member IsEnclosed : bool with get, set

/// <summary>Indicates that a property is assigned by a database sequence.</summary>
[<AttributeUsage(AttributeTargets.Property)>]
type SequenceAttribute = 
  inherit Attribute

  /// <summary>Initializes a SequenceAttribute instance.</summary>
  new : unit -> SequenceAttribute

  /// <summary>Gets and Sets the catalog name.</summary>
  member Catalog : string with get, set

  /// <summary>Gets and Sets the schema name.</summary>
  member Schema : string with get, set

  /// <summary>Gets and Sets the sequence name.</summary>
  member Name : string with get, set

  /// <summary>Gets and Sets whether names are enclosed or not.</summary>
  member IsEnclosed : bool with get, set

  /// <summary>Gets and Sets the interval between sequence numbers.</summary>
  member IncrementBy : int with get, set

/// <summary>Defines the kinds of primary key columns.</summary>
type IdKind =
  /// <summary>The primary key is assigned by the application.</summary>
  | Assigned = 0

  /// <summary>The primary key is assigned by the database identity feature.</summary>
  | Identity = 1

  /// <summary>The primary key is assigned by the database sequence feature.</summary>
  | Sequence = 2

/// <summary>Indicates that a property is mapped to a primary key column.</summary>
[<AttributeUsage(AttributeTargets.Property)>]
type IdAttribute = 
  inherit Attribute

  /// <summary>Initializes a IdAttribute instance.</summary>
  new : unit -> IdAttribute

  /// <summary>Initializes a IdAttribute instance with the IdKind.</summary>
  new : IdKind -> IdAttribute

  /// <summary>The kind of primary key.</summary>
  member Kind : IdKind

/// <summary>Defines the kinds of version columns.</summary>
type VersionKind =
  /// <summary>The version is incremented.</summary>
  | Incremented = 0

  /// <summary>The version is computed.</summary>
  | Computed = 1

/// <summary>Indicates that a property is mapped to a version column which is used for optimistic lock.</summary>
[<AttributeUsage(AttributeTargets.Property)>]
type VersionAttribute = 
  inherit Attribute

  /// <summary>Initializes a VersionAttribute instance.</summary>
  new : unit -> VersionAttribute

  /// <summary>Initializes a VersionAttribute instance with the VersionKind.</summary>
  new : VersionKind -> VersionAttribute

  /// <summary>The kind of version.</summary>
  member Kind : VersionKind

/// <summary>Indicates that a class is mapped to a stored procedure.</summary>
[<AttributeUsage(AttributeTargets.Class)>]
type ProcedureAttribute = 
  inherit Attribute

  /// <summary>Initializes a ProcedureAttribute instance.</summary>
  new : unit -> ProcedureAttribute

  /// <summary>Gets and Sets the catalog name.</summary>
  member Catalog : string with get, set

  /// <summary>Gets and Sets the schema name.</summary>
  member Schema : string with get, set

  /// <summary>Gets and Sets the stored procedure name.</summary>
  member Name : string with get, set

  /// <summary>Gets and Sets whether names are enclosed or not.</summary>
  member IsEnclosed : bool with get, set

/// <summary>Defines the kinds of stored procedure parameter directions.</summary>
type Direction =
  /// <summary>The parameter is an input parameter.</summary>
  | Input = 0

  /// <summary>The parameter is capable of both input and output.</summary>
  | InputOutput = 1

  /// <summary>The parameter is an output parameter.</summary>
  | Output = 2

  /// <summary>The parameter represents a return value from an operation such as a stored procedure, built-in function, or user-defined function.</summary>
  | ReturnValue = 3

  /// <summary>The parameter is a result set from an operation such as a stored procedure, built-in function, or user-defined function.</summary>
  /// <remarks>
  /// The field type must be F# list or System.Collections.Generics.IList.
  /// The element type of F# list or System.Collections.Generics.IList must be a type such as a basic type, a record type, a reference type or a tuple type.
  ///</remarks>
  | Result = 4

/// <summary>Indicates that a property is mapped to a stored procedure parameter.</summary>
[<AttributeUsage(AttributeTargets.Property)>]
type ProcedureParamAttribute = 
  inherit Attribute

  /// <summary>Initializes a ProcedureParamAttribute instance.</summary>
  new : unit -> ProcedureParamAttribute

  /// <summary>Gets and Sets the parameter name.</summary>
  member Name : string with get, set

  /// <summary>Gets and Sets the parameter direction.</summary>
  member Direction : Direction with get, set

  /// <summary>Gets and Sets the parameter size.</summary>
  member Size : int with get, set

  /// <summary>Gets and Sets the parameter precision.</summary>
  member Precision : byte with get, set

  /// <summary>Gets and Sets the parameter scale.</summary>
  member Scale : byte with get, set

  /// <summary>Gets and Sets the parameter scale.</summary>
  member Scale : byte with get, set

  /// <summary>Gets and Sets the user defined type name.</summary>
  member UdtTypeName : string with get, set

  member internal SizeOpt : int option
  member internal PrecisionOpt : byte option
  member internal ScaleOpt : byte option

/// <summary>Represents a SQL parameter.</summary>
type PreparedParameter =
  { /// <summary>Gets the parameter name.</summary>
    Name : string

    /// <summary>Gets the parameter value.</summary>
    Value : obj

    /// <summary>Gets the parameter CLR type.</summary>
    Type : Type

    /// <summary>Gets the parameter DB type.</summary>
    DbType: DbType

    /// <summary>Gets the parameter direction.</summary>
    Direction : Direction

    /// <summary>Gets the parameter size.</summary>
    Size : int option

    /// <summary>Gets the parameter precision.</summary>
    Precision : byte option

    /// <summary>Gets the parameter scale.</summary>
    Scale : byte option 
    
    /// <summary>Gets the user defined type name.</summary>
    UdtTypeName : string }

/// <summary>Represents a SQL statement.</summary>
type PreparedStatement =
  { /// <summary>Gets the SQL.</summary>
    Text : string

    /// <summary>Gets the formatted SQL.</summary>
    FormattedText : string

    /// <summary>Gets parameters.</summary>
    Parameters : PreparedParameter list }

/// <summary>Represents a SQL dialect.</summary>
[<Interface>]
type IDialect =

  /// <summary>Gets a value indicating whether the insert and the identity getting can be done at once.</summary>
  abstract CanGetIdentityAtOnce : bool

  /// <summary>Gets a value indicating whether the insert and the identity and version getting can be done at once.</summary>
  abstract CanGetIdentityAndVersionAtOnce : bool

  /// <summary>Gets a value indicating whether the insert and the version getting can be done at once.</summary>
  abstract CanGetVersionAtOnce : bool

  /// <summary>Gets a value indicating whether a result parameter is recognized as an output parameter.</summary>
  abstract IsResultParamRecognizedAsOutputParam : bool

  /// <summary>Gets a value indicating whether the System.Data.Common.DbDataReader.HasRows Property is supported.</summary>
  abstract IsHasRowsPropertySupported : bool

  /// <summary>Gets a root expression context.</summary>
  abstract RootExprCtxt : IDictionary<string, obj * Type>

  /// <summary>Escapes meta characters.</summary>
  /// <param name="text">The text.</param>
  /// <returns>The escaped text.</returns>
  abstract EscapeMetaChars : text:string -> string

  /// <summary>Prepares the identity select SQL statement.</summary>
  /// <param name="tableName">The table name.</param>
  /// <param name="idColumnName">The identity column name.</param>
  /// <returns>The SQL statement.</returns>
  abstract PrepareIdentitySelect : tableName:string * idColumnName:string -> PreparedStatement

  /// <summary>Prepares the identity and version select SQL statement.</summary>
  /// <param name="tableName">The table name.</param>
  /// <param name="idColumnName">The identity column name.</param>
  /// <param name="versionColumnName">The version column name.</param>
  /// <returns>The SQL statement.</returns>
  abstract PrepareIdentityAndVersionSelect : tableName:string * idColumnName:string * versionColumnName:string -> PreparedStatement

  /// <summary>Prepares the version select SQL statement.</summary>
  /// <param name="tableName">The table name.</param>
  /// <param name="versionColumnName">The version column name.</param>
  /// <param name="idMetaList">The list of primary key metadata.</param>
  /// <returns>The SQL statement.</returns>
  abstract PrepareVersionSelect : tableName:string * versionColumnName:string * idMetaList:list<string * obj * Type> -> PreparedStatement

  /// <summary>Prepares the sequence select SQL statement.</summary>
  /// <param name="sequenceName">The sequence name.</param>
  /// <returns>The SQL statement.</returns>
  abstract PrepareSequenceSelect : sequenceName:string  -> PreparedStatement

  /// <summary>Converts the value from the DB to the CLR.</summary>
  /// <param name="dbValue">The DB value.</param>
  /// <param name="destType">The destination CLR type.</param>
  /// <param name="udtTypeName">The user defined type name.</param>
  /// <param name="destProp">The destination property.</param>
  /// <returns>The converted value.</returns>
  abstract ConvertFromDbToClr : dbValue:obj * destType:Type * udtTypeName:string * destProp:PropertyInfo -> obj

  /// <summary>Converts the value from the CLR to the DB.</summary>
  /// <param name="clrValue">The CLR value.</param>
  /// <param name="srcType">The source CLR type.</param>
  /// <param name="udtTypeName">The user defined type name.</param>
  /// <returns>The converted value, the underlying CLR type and the DB type.</returns>
  abstract ConvertFromClrToDb : clrValue:obj * srcType:Type * udtTypeName:string -> obj * Type * DbType

  /// <summary>Formats the value as the SQL literal.</summary>
  /// <param name="dbValue">The DB value.</param>
  /// <param name="clrValue">The CLR type.</param>
  /// <param name="dbType">The DB type.</param>
  /// <returns>The converted value, the underlying CLR type and the DB type.</returns>
  abstract FormatAsSqlLiteral : dbValue:obj * clrType:Type * dbType:DbType -> string

  /// <summary>Create the parameter name.</summary>
  /// <param name="index">The index.</param>
  /// <returns>The parameter name.</returns>
  abstract CreateParameterName : index:int -> string

  /// <summary>Create the parameter name.</summary>
  /// <param name="baseName">The base name.</param>
  /// <returns>The parameter name.</returns>
  abstract CreateParameterName : baseName:string -> string

  /// <summary>Gets a value indicating whether the exception represents the unique constraint violation.</summary>
  abstract IsUniqueConstraintViolation : exn:exn -> bool

  /// <summary>Rewrites the SQL for pagination.</summary>
  /// <param name="statement">The SQL statement.</param>
  /// <param name="sql">The SQL.</param>  
  /// <param name="condition">The condition.</param>
  /// <param name="offset">The offset.</param>
  /// <param name="limit">The limit.</param>
  /// <returns>The rewrote SQL and condition.</returns>
  abstract RewriteForPagination : statement:SqlAst.Statement * sql:string * condition:IDictionary<string, obj * Type> * offset:int64 * limit:int64 -> string * IDictionary<string, obj * Type>

  /// <summary>Rewrites the SQL for pagination with the count calculation.</summary>
  /// <param name="statement">The SQL statement.</param>  
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <param name="offset">The offset.</param>
  /// <param name="limit">The limit.</param>
  /// <returns>The rewrote SQL and condition.</returns>
  abstract RewriteForCalcPagination : statement:SqlAst.Statement * sql:string * condition:IDictionary<string, obj * Type> * offset:int64 * limit:int64 -> string * IDictionary<string, obj * Type>

  /// <summary>Rewrites the SQL for counting.</summary>
  /// <param name="statement">The SQL statement.</param>
  /// <param name="sql">The SQL.</param>
  /// <param name="condition">The condition.</param>
  /// <returns>The rewrote SQL and condition.</returns>
  abstract RewriteForCount : statement:SqlAst.Statement * sql:string * condition:IDictionary<string, obj * Type> -> string * IDictionary<string, obj * Type>

  /// <summary>Builds the stored procedure call SQL.</summary>
  /// <param name="procedureName">The stored procedure name.</param>
  /// <param name="parameters">SQL parameters.</param>
  /// <returns>The built SQL.</returns>
  abstract BuildProcedureCallSql : procedureName:string * parameters:PreparedParameter seq -> string
  
  /// <summary>Encloses the identifier.</summary>
  /// <returns>The enclosed identifier.</returns>
  abstract EncloseIdentifier : identifier:string -> string

  /// <summary>Setups the <c>System.Data.Common.DbParameter</c>.</summary>
  /// <param name="param">The Soma SQL parameter.</param>
  /// <param name="dbParam">The ADO.NET DB parameter.</param>
  abstract SetupDbParameter : param:PreparedParameter * dbParam:DbParameter -> unit

/// <summary>Represents a <c>System.Data.Common.DbConnection</c> Observer.</summary>
type IConnectionObserver = 

  /// <summary>Notifies that <c>System.Data.Common.DbConnection</c> is opening.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="userState">The user state.</param>
  abstract NotifyOpening : connection:DbConnection * [<System.Runtime.InteropServices.Out>]userState:byref<obj> -> unit

  /// <summary>Notifies that <c>System.Data.Common.DbConnection</c> is opened.</summary>
  /// <param name="connection">The connection.</param>
  /// <param name="userState">The user state.</param>
  abstract NotifyOpened : connection:DbConnection * userState:obj -> unit

/// <summary>Represents a <c>System.Data.Common.DbCommand</c> Observer.</summary>
type ICommandObserver =

  /// <summary>Notifies that <c>System.Data.Common.DbCommand</c> is executing.</summary>
  /// <param name="command">The command.</param>
  /// <param name="statement">The SQL statement.</param>
  /// <param name="userState">The user state.</param>
  abstract NotifyExecuting : command:DbCommand * statement:PreparedStatement * [<System.Runtime.InteropServices.Out>]userState:byref<obj> -> unit

  /// <summary>Notifies that <c>System.Data.Common.DbCommand</c> is executed.</summary>
  /// <param name="command">The command.</param>
  /// <param name="statement">The SQL statement.</param>
  /// <param name="userState">The user state.</param>
  abstract NotifyExecuted : command:DbCommand * statement:PreparedStatement * userState:obj -> unit

/// <summary>Represents a database configuration.</summary>
[<Interface>]
type IDbConfig =
  /// <summary>Gets the invariant name of a provider.</summary>
  abstract Invariant : string

  /// <summary>Gets the db provider factory.</summary>
  abstract DbProviderFactory : DbProviderFactory

  /// <summary>Gets the string used to open the connection.</summary>
  abstract ConnectionString : string

  /// <summary>Gets the SQL dialect.</summary>
  abstract Dialect : IDialect

  /// <summary>Gets the SQL Parser.</summary>
  abstract SqlParser : Func<string, SqlAst.Statement>

  /// <summary>Gets the expression Parser.</summary>
  abstract ExpressionParser : Func<string, ExpressionAst.Expression>

  /// <summary>Gets the SQL Logger.</summary>
  abstract Logger : Action<PreparedStatement>

  /// <summary>Gets the Connection Observer.</summary>
  abstract ConnectionObserver : IConnectionObserver

  /// <summary>Gets the Command Observer.</summary>
  abstract CommandObserver : ICommandObserver
