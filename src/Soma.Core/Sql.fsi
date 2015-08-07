﻿//----------------------------------------------------------------------------
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

/// <summary>The exception that is thrown when there is no insertable property.</summary>
exception NoInsertablePropertyException of unit

/// <summary>The exception that is thrown when there is no updatable property.</summary>
exception NoUpdatablePropertyException of unit

/// <summary>The exception that is thrown when there is no id property.</summary>
exception NoIdPropertiesException of string

/// <summary>Represents the fixed length string.</summary>
[<Class>]
[<Sealed>]
type CharString = 
  interface IComparable<CharString>

  /// <summary>Initializes a CharString instance.</summary>
  new : string -> CharString

  /// <summary>Gets the string value.</summary>
  member Value : string with get

  /// <summary>Determines whether two specified CharString objects have the same value.</summary>
  static member op_Equality : CharString * CharString -> bool

  /// <summary>Determines whether two specified CharString objects have the different value.</summary>
  static member op_Inequality : CharString * CharString -> bool

/// <summary>Represents the options of insert operation.</summary>

type IInsertOrUpdateOpt =
    abstract member Exclude : string seq
    abstract member Include : string seq
    abstract member ExcludeNull : bool

[<Class>]
type InsertOpt =
  interface IInsertOrUpdateOpt
  /// <summary>Initializes a InsertOpt instance.</summary>
  new : unit -> InsertOpt

  /// <summary>Gets and Sets property names that should be excluded.</summary>
  member Exclude : seq<string> with get, set

  /// <summary>Gets and Sets property names that should be included.</summary>
  member Include : seq<string> with get, set

  /// <summary>Gets and Sets whether null properties are excluded.</summary>
  member ExcludeNull : bool with get, set

/// <summary>Represents the options of update operation.</summary>
[<Class>]
type UpdateOpt =
  interface IInsertOrUpdateOpt
  /// <summary>Initializes a UpdateOpt instance.</summary>
  new : unit -> UpdateOpt

  /// <summary>Gets and Sets property names that should be excluded.</summary>
  member Exclude : seq<string> with get, set

  /// <summary>Gets and Sets property names that should be included.</summary>
  member Include : seq<string> with get, set

  /// <summary>Gets and Sets whether null properties are excluded.</summary>
  member ExcludeNull : bool with get, set

  /// <summary>Gets and Sets whether the version property are ignored.</summary>
  member IgnoreVersion : bool with get, set

/// <summary>Represents the options of delete operation.</summary>
[<Class>]
type DeleteOpt =
  /// <summary>Initializes a DeleteOpt instance.</summary>
  new : unit -> DeleteOpt

  /// <summary>Gets and Sets whether the version property are ignored.</summary>
  member IgnoreVersion : bool with get, set

type internal SqlException =
  inherit InvalidOperationException
  new : message:Message * ?innerException:exn -> SqlException
  member MessageId : string

/// <summary>Provides the SQL operations.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sql =
  
  open Soma.Core.Text
  open Soma.Core.SqlAst

  /// <summary>Parses the SQL.</summary>
  /// <param name="sql">The SQL.</param>
  /// <returns>The parsed SQL statement.</returns>
  [<CompiledName("Parse")>]
  val parse : string -> Statement
  val internal resolveOrderByEmbeddedVariables : IDbConfig -> Statement -> string -> IDictionary<string, obj * Type> -> string
  val internal prepare : IDbConfig -> string -> IDictionary<string, obj * Type> -> Func<string, Statement> -> PreparedStatement
  val internal preparePaginate : IDbConfig -> string -> IDictionary<string, obj * Type> -> int64 -> int64 -> Func<string, Statement> -> PreparedStatement
  val internal preparePaginateAndCount : IDbConfig -> string -> IDictionary<string, obj * Type> -> int64 -> int64 -> Func<string, Statement> -> PreparedStatement * PreparedStatement
  val internal prepareFind : IDbConfig -> obj list -> EntityMeta -> PreparedStatement
  val internal prepareInsert : IDbConfig -> obj -> EntityMeta -> InsertOpt -> PreparedStatement
  val internal prepareUpdate : IDbConfig -> obj -> EntityMeta -> UpdateOpt -> PreparedStatement
  val internal prepareInsertOrUpdate : IDbConfig -> obj -> obj -> EntityMeta -> UpdateOpt -> PropMeta option -> PropMeta option -> PreparedStatement
  val internal prepareDelete : IDbConfig -> obj -> EntityMeta -> DeleteOpt -> PreparedStatement
  val internal prepareCall : IDbConfig -> obj -> ProcedureMeta -> PreparedStatement

/// <summary>Represents a base class of the Soma.Core.IDialect.</summary>
[<AbstractClass>]
type DialectBase =
  interface IDialect
  new : Func<Type, Data.DbType option> -> DialectBase
  member ConcatExprCtxt : IDictionary<string, obj * Type> * IDictionary<string, obj * Type> -> IDictionary<string, obj * Type>
  abstract CanGetIdentityAtOnce : bool
  default CanGetIdentityAtOnce : bool
  abstract CanGetIdentityAndVersionAtOnce : bool
  default CanGetIdentityAndVersionAtOnce : bool
  abstract CanGetVersionAtOnce : bool
  default CanGetVersionAtOnce : bool
  abstract IsResultParamRecognizedAsOutputParam : bool
  default IsResultParamRecognizedAsOutputParam : bool
  abstract IsHasRowsPropertySupported : bool
  default IsHasRowsPropertySupported : bool
  abstract RootExprCtxt : IDictionary<string, obj * Type>
  default RootExprCtxt : IDictionary<string, obj * Type>
  abstract CountFunction : string
  default CountFunction : string
  abstract EscapeMetaChars : string -> string
  abstract PrepareIdentitySelect : string * string -> PreparedStatement
  default PrepareIdentitySelect : string * string -> PreparedStatement
  abstract PrepareIdentityAndVersionSelect : string * string * string -> PreparedStatement
  default PrepareIdentityAndVersionSelect : string * string * string -> PreparedStatement
  abstract PrepareVersionSelect : string * string * list<string * obj * Type> -> PreparedStatement
  default PrepareVersionSelect : string * string * list<string * obj * Type> -> PreparedStatement
  abstract PrepareSequenceSelect : string  -> PreparedStatement
  default PrepareSequenceSelect : string  -> PreparedStatement
  abstract MapClrTypeToDbType : Type -> DbType
  default MapClrTypeToDbType : Type -> DbType
  abstract ConvertFromDbToUnderlyingClr : obj * Type -> obj
  default ConvertFromDbToUnderlyingClr : obj * Type -> obj
  abstract ConvertFromDbToClr : obj * Type * string * PropertyInfo -> obj
  default ConvertFromDbToClr : obj * Type * string * PropertyInfo -> obj
  abstract ConvertFromClrToDb : obj * Type * string -> obj * Type * DbType
  default ConvertFromClrToDb : obj * Type * string -> obj * Type * DbType
  abstract FormatAsSqlLiteral : obj * Type * DbType -> string
  default FormatAsSqlLiteral : obj * Type * DbType -> string
  abstract CreateParameterName : int -> string
  default CreateParameterName : int -> string
  abstract CreateParameterName : string -> string
  default CreateParameterName : string -> string
  abstract IsUniqueConstraintViolation : exn -> bool
  abstract RewriteForPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  default RewriteForPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  abstract RewriteForCalcPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  default RewriteForCalcPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  abstract RewriteForCount : SqlAst.Statement * string * IDictionary<string, obj * Type> -> string * IDictionary<string, obj * Type>
  default RewriteForCount : SqlAst.Statement * string * IDictionary<string, obj * Type> -> string * IDictionary<string, obj * Type>
  abstract BuildProcedureCallSql : string * PreparedParameter seq -> string
  abstract EncloseIdentifier : string -> string
  abstract SetupDbParameter : PreparedParameter * DbParameter -> unit
  default SetupDbParameter : PreparedParameter * DbParameter -> unit
  abstract GetValue : DbDataReader * int * PropertyInfo -> obj
  default GetValue : DbDataReader * int * PropertyInfo -> obj
  abstract MakeParametersDisposer : DbCommand -> IDisposable
  default MakeParametersDisposer : DbCommand -> IDisposable

/// <summary>Represents a SQL dialect of Microsoft SQL Server 2008.</summary>
type MsSqlDialect =
  inherit DialectBase
  new : Func<Type, Data.DbType option> -> MsSqlDialect
  override CanGetIdentityAtOnce : bool
  override CanGetIdentityAndVersionAtOnce : bool
  override CanGetVersionAtOnce : bool
  override CountFunction : string
  override EscapeMetaChars : string -> string
  override PrepareIdentityAndVersionSelect : string  * string * string -> PreparedStatement
  override PrepareVersionSelect : string  * string * list<string * obj * Type> -> PreparedStatement
  override IsUniqueConstraintViolation : exn -> bool
  override RewriteForPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  override RewriteForCalcPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  override BuildProcedureCallSql : string * PreparedParameter seq -> string
  override EncloseIdentifier : string -> string
  override SetupDbParameter : PreparedParameter * DbParameter -> unit

/// <summary>Represents a SQL dialect of Microsoft SQL Server Compact 4.0.</summary>
type MsSqlCeDialect =
  inherit DialectBase
  new : Func<Type, Data.DbType option> -> MsSqlCeDialect
  override CountFunction : string
  override EscapeMetaChars : string -> string
  override PrepareIdentitySelect : string  * string -> PreparedStatement
  override PrepareIdentityAndVersionSelect : string  * string * string -> PreparedStatement
  override PrepareVersionSelect : string  * string * list<string * obj * Type> -> PreparedStatement
  override IsUniqueConstraintViolation : exn -> bool
  override RewriteForPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  override RewriteForCalcPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  override BuildProcedureCallSql : string * PreparedParameter seq -> string
  override EncloseIdentifier : string -> string
  override IsHasRowsPropertySupported : bool

/// <summary>Represents a SQL dialect of MySQL 5.x.</summary>
type MySqlDialect =
  inherit DialectBase
  new : Func<Type, Data.DbType option> -> MySqlDialect
  override EscapeMetaChars : string -> string
  override PrepareIdentitySelect : string  * string -> PreparedStatement
  override IsUniqueConstraintViolation : exn -> bool
  override RewriteForPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  override RewriteForCalcPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  override RewriteForCount : SqlAst.Statement * string * IDictionary<string, obj * Type> -> string * IDictionary<string, obj * Type>
  override BuildProcedureCallSql : string * PreparedParameter seq -> string
  override EncloseIdentifier : string -> string
  override SetupDbParameter : PreparedParameter * DbParameter -> unit

/// <summary>Represents a SQL dialect of Oracle Database 11g.</summary>
type OracleDialect =
  inherit DialectBase
  new : Func<Type, Data.DbType option> -> OracleDialect
  override EscapeMetaChars : string -> string
  override PrepareSequenceSelect : string  -> PreparedStatement
  override ConvertFromDbToUnderlyingClr : obj * Type -> obj
  override ConvertFromClrToDb : obj * Type * string -> obj * Type * DbType
  override FormatAsSqlLiteral : obj * Type * DbType -> string
  override IsUniqueConstraintViolation : exn -> bool
  override CreateParameterName : int -> string
  override CreateParameterName : string -> string
  override BuildProcedureCallSql : string * PreparedParameter seq -> string
  override EncloseIdentifier : string -> string
  override SetupDbParameter : PreparedParameter * DbParameter -> unit
  override MakeParametersDisposer : DbCommand -> IDisposable

/// <summary>Represents a SQL dialect of MySQL 5.x.</summary>
type SQLiteDialect =
  inherit DialectBase
  new : Func<Type, Data.DbType option> -> SQLiteDialect
  override EscapeMetaChars : string -> string
  override PrepareIdentitySelect : string  * string -> PreparedStatement
  override ConvertFromDbToUnderlyingClr : obj * Type -> obj
  override IsUniqueConstraintViolation : exn -> bool
  override RewriteForPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  override RewriteForCalcPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  override BuildProcedureCallSql : string * PreparedParameter seq -> string
  override EncloseIdentifier : string -> string
  override FormatAsSqlLiteral : obj * Type * DbType -> string