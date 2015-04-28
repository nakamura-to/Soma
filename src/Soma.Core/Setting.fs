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

[<AttributeUsage(AttributeTargets.Class)>]
type TableAttribute private (name : string option) = 
  inherit Attribute()
  
  let mutable catalog:string = null
  let mutable schema:string = null
  let mutable name:string = 
    match name with 
    | Some n -> n
    | None -> null
  let mutable isEnclosed:bool = false

  new() = TableAttribute(None)
  new(name : string) = TableAttribute(Some name)

  member this.Catalog 
    with get () = catalog
    and  set (v) = catalog <- v
  member this.Schema 
    with get () = schema
    and  set (v) = schema <- v
  member this.Name
    with get () = name
    and  set (v) = name <- v
  member this.IsEnclosed
    with get () = isEnclosed
    and  set (v) = isEnclosed <- v

[<AttributeUsage(AttributeTargets.Property)>]
type ColumnAttribute private (name : string option) = 
  inherit Attribute()
  let mutable name:string = 
    match name with 
    | Some n -> n
    | None -> null
  let mutable insertable:bool = true
  let mutable updatable:bool = true
  let mutable isEnclosed:bool = false

  new() = ColumnAttribute(None)
  new(name : string) = ColumnAttribute(Some name)

  member this.Name
    with get () = name
    and  set (v) = name <- v
  member this.Insertable 
    with get () = insertable
    and  set (v) = insertable <- v
  member this.Updatable 
    with get () = updatable
    and  set (v) = updatable <- v
  member this.IsEnclosed
    with get () = isEnclosed
    and  set (v) = isEnclosed <- v

[<AttributeUsage(AttributeTargets.Property)>]
type SequenceAttribute() = 
  inherit Attribute()
  let mutable catalog:string = null
  let mutable schema:string = null
  let mutable name:string = null
  let mutable isEnclosed:bool = false
  let mutable incrementBy:int = 1
  member this.Catalog 
    with get () = catalog
    and  set (v) = catalog <- v
  member this.Schema 
    with get () = schema
    and  set (v) = schema <- v
  member this.Name
    with get () = name
    and  set (v) = name <- v
  member this.IsEnclosed
    with get () = isEnclosed
    and  set (v) = isEnclosed <- v
  member this.IncrementBy
    with get () = incrementBy
    and  set (v) = incrementBy <- v

type IdKind =
  | Assigned = 0
  | Identity = 1
  | Sequence = 2

[<AttributeUsage(AttributeTargets.Property)>]
type IdAttribute(kind:IdKind) = 
  inherit Attribute()
  member this.Kind = kind
  new () = new IdAttribute(IdKind.Assigned)

type VersionKind =
  | Incremented = 0
  | Computed = 1

[<AttributeUsage(AttributeTargets.Property)>]
type VersionAttribute(kind:VersionKind) = 
  inherit Attribute()
  member this.Kind = kind
  new () = new VersionAttribute(VersionKind.Incremented)

[<AttributeUsage(AttributeTargets.Class)>]
type ProcedureAttribute() = 
  inherit Attribute()
  let mutable catalog:string = null
  let mutable schema:string = null
  let mutable name:string = null
  let mutable isEnclosed:bool = false
  member this.Catalog 
    with get () = catalog
    and  set (v) = catalog <- v
  member this.Schema 
    with get () = schema
    and  set (v) = schema <- v
  member this.Name
    with get () = name
    and  set (v) = name <- v
  member this.IsEnclosed
    with get () = isEnclosed
    and  set (v) = isEnclosed <- v

type Direction =
  | Input = 0
  | InputOutput = 1
  | Output = 2
  | ReturnValue = 3
  | Result = 4

[<AttributeUsage(AttributeTargets.Property)>]
type ProcedureParamAttribute() = 
  inherit Attribute()
  let mutable name:string = null
  let mutable direction:Direction = Direction.Input
  let mutable sizeOpt:int option = None
  let mutable precisionOpt:byte option = None
  let mutable scaleOpt:byte option = None
  let mutable udtTypeName:string = null
  member this.Name
    with get () = name
    and  set (v) = name <- v
  member this.Direction
    with get () = direction
    and  set (v) = direction <- v
  member this.Size
    with get () = match sizeOpt with Some v -> v | _ -> 0
    and  set (v) = sizeOpt <- Some v
  member this.Precision
    with get () = match precisionOpt with Some v -> v | _ -> 0uy
    and  set (v) = precisionOpt <- Some v
  member this.Scale
    with get () = match scaleOpt with Some v -> v | _ -> 0uy
    and  set (v) = scaleOpt <- Some v
  member this.UdtTypeName
    with get () = udtTypeName
    and  set (v) = udtTypeName <- v
  member this.SizeOpt = sizeOpt
  member this.PrecisionOpt = precisionOpt
  member this.ScaleOpt = scaleOpt

type PreparedParameter =
  { Name : string
    Value : obj
    Type : Type
    DbType: DbType
    Direction : Direction
    Size : int option
    Precision : byte option
    Scale : byte option
    UdtTypeName : string }
  override this.ToString() =
    string this.Value

type PreparedStatement =
  { Text : string
    FormattedText : string
    Parameters : PreparedParameter list }

type IDialect =
  abstract CanGetIdentityAtOnce : bool
  abstract CanGetIdentityAndVersionAtOnce : bool
  abstract CanGetVersionAtOnce : bool
  abstract IsResultParamRecognizedAsOutputParam : bool
  abstract IsHasRowsPropertySupported : bool
  abstract RootExprCtxt : IDictionary<string, obj * Type>
  abstract EscapeMetaChars : string -> string
  abstract PrepareIdentitySelect : string * string -> PreparedStatement
  abstract PrepareIdentityAndVersionSelect : string * string * string -> PreparedStatement
  abstract PrepareVersionSelect : string * string * list<string * obj * Type> -> PreparedStatement
  abstract PrepareSequenceSelect : string  -> PreparedStatement
  abstract ConvertFromDbToClr : obj * Type * string * PropertyInfo-> obj
  abstract ConvertFromClrToDb : obj * Type * string -> obj * Type * DbType
  abstract FormatAsSqlLiteral : obj * Type * DbType -> string
  abstract CreateParameterName : int -> string
  abstract CreateParameterName : string -> string
  abstract IsUniqueConstraintViolation : exn -> bool
  abstract RewriteForPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  abstract RewriteForCalcPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  abstract RewriteForCount : SqlAst.Statement * string * IDictionary<string, obj * Type> -> string * IDictionary<string, obj * Type>
  abstract BuildProcedureCallSql : string * PreparedParameter seq -> string
  abstract EncloseIdentifier : string -> string
  abstract SetupDbParameter : PreparedParameter * DbParameter -> unit
  abstract GetValue : DbDataReader * int * PropertyInfo -> obj
  abstract MakeParametersDisposer : DbCommand -> IDisposable

type IConnectionObserver = 
  abstract NotifyOpening : connection:DbConnection * [<System.Runtime.InteropServices.Out>]userState:byref<obj> -> unit
  abstract NotifyOpened : connection:DbConnection * userState:obj -> unit

type ICommandObserver =
  abstract NotifyExecuting : command:IDbCommand * [<System.Runtime.InteropServices.Out>]userState:byref<obj> -> unit
  abstract NotifyExecuted : command:IDbCommand * userState:obj -> unit

type IDbConfig =
  abstract Invariant : string
  abstract DbProviderFactory : DbProviderFactory
  abstract ConnectionString : string
  abstract Dialect : IDialect
  abstract SqlParser : Func<string, SqlAst.Statement>
  abstract QueryTranslator : IDbConnection -> System.Linq.Expressions.Expression -> IDbCommand * FSharp.QueryProvider.DataReader.TypeConstructionInfo
  abstract ExpressionParser : Func<string, ExpressionAst.Expression>
  abstract Logger : Action<PreparedStatement>
  abstract ConnectionObserver : IConnectionObserver
  abstract CommandObserver : ICommandObserver
