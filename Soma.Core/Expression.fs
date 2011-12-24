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
open System.Reflection
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open Microsoft.FSharp.Reflection
open Soma.Core
open Soma.Core.Text
open Soma.Core.ExpressionAst
open Soma.Core.ExpressionParser

type internal ExpressionException (message:Message, ?innerException:exn) =
  inherit InvalidOperationException (message.Format (), match innerException with Some ex -> ex | _ -> null)
  member this.MessageId = message.Id

exception internal ExpressionGenerateError of Message * Location * exn option

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Expression = 

  let startLocation = { pos_bol = 0; pos_fname = "Expression"; pos_cnum = 0; pos_lnum = 1 }

  let getErrorMessage (ctxt:ParseErrorContext<token>) = 
    match ctxt.CurrentToken with
    | None -> 
      SR.SOMA1014 ctxt.Message
    | Some token -> 
      SR.SOMA1019 (token_to_string token) 

  let getLocation (lexbuf:LexBuffer<char>) =
    let pos = lexbuf.StartPos
    { Location.pos_bol = pos.pos_bol; pos_fname= pos.pos_fname; pos_cnum = pos.pos_cnum; pos_lnum = pos.pos_lnum }

  let appendDetail message (text:string) loc = 
     { message with Text = message.Text + " " + (SR.SOMA1020 (text, loc.pos_lnum, loc.pos_cnum)).Text }

  [<CompiledName("Parse")>]
  let parse expr =
    let lexbuf = LexBuffer<char>.FromString(expr)
    let loc = startLocation
    lexbuf.EndPos <- { pos_bol = loc.pos_bol; pos_fname = loc.pos_fname; pos_cnum = loc.pos_cnum; pos_lnum = loc.pos_lnum } 
    try 
      ExpressionParser.start ExpressionLexer.tokenize lexbuf
    with 
    | ExpressionParseError (message, loc) -> 
      let message = appendDetail message expr loc
      raise <| ExpressionException (message)
    | ExpressionParseErrorWithContext obj ->
      let ctxt = obj :?> ParseErrorContext<token>
      let message = getErrorMessage ctxt
      let loc = getLocation lexbuf
      let message = appendDetail message expr loc
      raise <| ExpressionException (message) 
    | ex -> 
      let loc = getLocation lexbuf
      let message = appendDetail (SR.SOMA1015 ()) expr loc
      raise <| ExpressionException (message, ex)

  let (|GetType|_|) typeName =
    let resolve =
      function
      | "bool" -> "System.Boolean"
      | "byte" -> "System.Byte"
      | "sbyte" -> "System.SByte"
      | "int16" -> "System.Int16"
      | "uint16" -> "System.UInt16"
      | "int" | "int32" -> "System.Int32"
      | "uint" -> "System.UInt32"
      | "int64" -> "System.Int64"
      | "uint64" -> "System.UInt64"
      | "nativeint" -> "System.IntPtr"
      | "unativeint" -> "System.UIntPtr"
      | "single" | "float32" -> "System.Single"
      | "double" | "float" -> "System.Double"
      | "decimal" -> "System.Decimal"
      | "bigint" -> "Math.BigInt"
      | "bignum" -> "Math.BigNum"
      | "unit" -> "Core.Unit"
      | x -> x
    match Type.GetType(resolve typeName) with
    | null -> None 
    | typ -> Some typ

  let (|GetNullableProperty|_|) (obj, typ:Type, name) =
    if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Nullable<_>> then
      let underlyingType = Nullable.GetUnderlyingType(typ)
      match obj, name with
      | null, "HasValue" -> Some (box false, typeof<bool>)
      | null, "Value" -> Some (null, underlyingType)
      | _, "HasValue" -> Some (box true, typeof<bool>)
      | _, "Value" -> Some (obj, underlyingType)
      | _ -> None
    else 
      None

  let (|GetOptionProperty|_|) (obj, typ:Type, name) =
    if Reflection.isOptionType typ then
      match typ.GetProperty(name) with 
      | null -> None
      | prop -> 
        match prop.GetGetMethod() with
        | null -> None
        | m when m.IsStatic -> Some (prop.GetValue(null, [| obj |]), prop.PropertyType)
        | _ -> None
    else 
      None

  let (|GetDictValue|_|) (obj:obj, typ, name) =
    if Reflection.isGenericDictionary typ then
      match Reflection.getGenericDictionaryValue(name, obj, typ) with
      | Some value -> Some (value, typeof<obj>)
      | _ -> None
    elif typeof<IDictionary>.IsAssignableFrom(typ) then
      let dict = obj :?> IDictionary
      Some (dict.[name], typeof<obj>)
    else 
      None

  let (|GetProperty|_|) (obj, typ:Type, name) =
      match typ.GetProperty(name) with 
      | null -> None 
      | prop -> Some (prop.GetValue(obj, null), prop.PropertyType)

  let (|GetStaticProperty|_|) (typ:Type, name) =
      match typ.GetProperty(name) with 
      | null -> None 
      | prop -> Some (prop.GetValue(null, null), prop.PropertyType)

  let (|GetField|_|) (obj, typ:Type, name) =
      match typ.GetField(name) with 
      | null -> None 
      | field -> Some (field.GetValue(obj), field.FieldType)

  let (|GetStaticField|_|) (typ:Type, name) =
      match typ.GetField(name) with 
      | null -> None 
      | field -> Some (field.GetValue(null), field.FieldType)

  let tryCast<'T> (expr:obj * Type) =
    if typeof<'T>.IsAssignableFrom (snd expr) then
      Some (fst expr :?> 'T)
    else 
      None

  let (|Int32Op|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<int32> lhs
      let! rhs = tryCast<int32> rhs
      return (box (op lhs rhs), typeof<int32>) }

  let (|StringOp|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<string> lhs
      let! rhs = tryCast<string> rhs
      return (box (op lhs rhs), typeof<string>) }

  let (|FloatOp|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<float> lhs
      let! rhs = tryCast<float> rhs
      return (box (op lhs rhs), typeof<float>) }

  let (|Float32Op|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<float32> lhs
      let! rhs = tryCast<float32> rhs
      return (box (op lhs rhs), typeof<float32>) }

  let (|Int64Op|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<int64> lhs
      let! rhs = tryCast<int64> rhs
      return (box (op lhs rhs), typeof<int64>) }
  
  let (|UInt64Op|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<uint64> lhs
      let! rhs = tryCast<uint64> rhs
      return (box (op lhs rhs), typeof<uint64>) }

  let (|UInt32Op|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<uint32> lhs
      let! rhs = tryCast<uint32> rhs
      return (box (op lhs rhs), typeof<uint32>) }

  let (|Int16Op|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<int16> lhs
      let! rhs = tryCast<int16> rhs
      return (box (op lhs rhs), typeof<int16>) }

  let (|UInt16Op|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<uint16> lhs
      let! rhs = tryCast<uint16> rhs
      return (box (op lhs rhs), typeof<uint16>) }

  let (|SByteOp|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<sbyte> lhs
      let! rhs = tryCast<sbyte> rhs
      return (box (op lhs rhs), typeof<sbyte>) }

  let (|ByteOp|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<byte> lhs
      let! rhs = tryCast<byte> rhs
      return (box (op lhs rhs), typeof<byte>) }

  let (|DecimalOp|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<decimal> lhs
      let! rhs = tryCast<decimal> rhs
      return (box (op lhs rhs), typeof<decimal>) }

  let (|IComparableOp|_|) op (lhs:obj * Type, rhs:obj * Type) =
    maybe {
      let! lhs = tryCast<IComparable> lhs
      let! rhs = tryCast<IComparable> rhs
      if lhs.GetType() = rhs.GetType() then 
        return (box (op lhs rhs), typeof<bool>) }

  let applyAdd (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | Int32Op (+) (result) -> result
    | Int64Op (+) (result) -> result
    | StringOp (+) (result) -> result
    | DecimalOp (+) (result) -> result
    | FloatOp (+) (result) -> result
    | Float32Op (+) (result) -> result
    | UInt64Op (+) (result) -> result
    | UInt32Op (+) (result) -> result
    | Int16Op (+) (result) -> result
    | UInt16Op (+) (result) -> result
    | SByteOp (+) (result) -> result
    | ByteOp (+) (result) -> result
    | _ ->
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 ("+", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1002 ("+", lhsType.FullName), loc, None)

  let applySub (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | Int32Op (-) (result) -> result
    | Int64Op (-) (result) -> result
    | DecimalOp (-) (result) -> result
    | FloatOp (-) (result) -> result
    | Float32Op (-) (result) -> result
    | UInt64Op (-) (result) -> result
    | UInt32Op (-) (result) -> result
    | Int16Op (-) (result) -> result
    | UInt16Op (-) (result) -> result
    | SByteOp (-) (result) -> result
    | ByteOp (-) (result) -> result
    | _ ->
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 ("-", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1002 ("-", lhsType.FullName), loc, None)

  let applyMul (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | Int32Op (*) (result) -> result
    | Int64Op (*) (result) -> result
    | DecimalOp (*) (result) -> result
    | FloatOp (*) (result) -> result
    | Float32Op (*) (result) -> result
    | UInt64Op (*) (result) -> result
    | UInt32Op (*) (result) -> result
    | Int16Op (*) (result) -> result
    | UInt16Op (*) (result) -> result
    | SByteOp (*) (result) -> result
    | ByteOp (*) (result) -> result
    | _ ->
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 ("*", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1002 ("*", lhsType.FullName), loc, None)

  let applyDiv (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | Int32Op (/) (result) -> result
    | Int64Op (/) (result) -> result
    | DecimalOp (/) (result) -> result
    | FloatOp (/) (result) -> result
    | Float32Op (/) (result) -> result
    | UInt64Op (/) (result) -> result
    | UInt32Op (/) (result) -> result
    | Int16Op (/) (result) -> result
    | UInt16Op (/) (result) -> result
    | SByteOp (/) (result) -> result
    | ByteOp (/) (result) -> result
    | _ ->
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 ("/", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1002 ("/", lhsType.FullName), loc, None)

  let applyMod (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | Int32Op (%) (result) -> result
    | Int64Op (%) (result) -> result
    | DecimalOp (%) (result) -> result
    | FloatOp (%) (result) -> result
    | Float32Op (%) (result) -> result
    | UInt64Op (%) (result) -> result
    | UInt32Op (%) (result) -> result
    | Int16Op (%) (result) -> result
    | UInt16Op (%) (result) -> result
    | SByteOp (%) (result) -> result
    | ByteOp (%) (result) -> result
    | _ ->
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 ("%", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1002 ("%", lhsType.FullName), loc, None)

  let applyEqual (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    box ((fst lhs) = (fst rhs)), typeof<bool>

  let applyNotEqual (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    box ((fst lhs) <> (fst rhs)), typeof<bool>

  let applyLessThan (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | IComparableOp (<) (result) -> result
    | _ -> 
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 ("<", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1007 ("<", lhsType.FullName), loc, None)

  let applyGreaterThan (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | IComparableOp (>) (result) -> result
    | _ -> 
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 (">", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1007 (">", lhsType.FullName), loc, None)

  let applyLessThanOrEqual (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | IComparableOp (<=) (result) -> result
    | _ -> 
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 ("<=", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1007 ("<=", lhsType.FullName), loc, None)

  let applyGreaterThanOrEqual (loc:Location) (lhs:obj * Type, rhs:obj * Type) =
    match lhs, rhs with
    | IComparableOp (>=) (result) -> result
    | _ -> 
      let lhsType = snd lhs
      let rhsType = snd rhs
      if lhsType <> rhsType then
        raise <| ExpressionGenerateError (SR.SOMA1001 (">=", lhsType.FullName, rhsType.FullName), loc, None)
      else
        raise <| ExpressionGenerateError (SR.SOMA1007 (">=", lhsType.FullName), loc, None)

  let applyAndAlso (loc:Location) (lhs:obj * Type, rhs:Lazy<obj * Type>) =
    match tryCast<bool> lhs with
    | Some result ->
      if result then
        match tryCast<bool> rhs.Value with
        | Some result ->
          if result then
            box true, typeof<bool>
          else 
            box false, typeof<bool>
        | _ -> 
          raise <| ExpressionGenerateError (SR.SOMA1009 ("&&", (snd rhs.Value).FullName), loc, None)
      else 
        box false, typeof<bool>
    | _ ->
        raise <| ExpressionGenerateError (SR.SOMA1009 ("&&", (snd lhs).FullName), loc, None)
      
  let applyOrElse (loc:Location) (lhs:obj * Type, rhs:Lazy<obj * Type>) =
    match tryCast<bool> lhs with
    | Some result ->
      if result then
        box true, typeof<bool>
      else 
        match tryCast<bool> rhs.Value with
        | Some result ->
          if result then
            box true, typeof<bool>
          else 
            box false, typeof<bool>
        | _ -> 
          raise <| ExpressionGenerateError (SR.SOMA1009 ("||", (snd rhs.Value).FullName), loc, None)
    | _ ->
        raise <| ExpressionGenerateError (SR.SOMA1009 ("||", (snd lhs).FullName), loc, None)

  let applyNot op (loc:Location) (value:obj * Type)  =
    match tryCast<bool> value with
    | Some result -> 
      box (not result), typeof<bool> 
    | _ -> 
      raise <| ExpressionGenerateError (SR.SOMA1011 (op, snd value), loc, None)

  let applyApplication (loc:Location) (func:obj, funcType:Type) (arg:obj, argType:Type) =
    let invokeMethod = funcType.GetMethod("Invoke", [| argType |])
    if invokeMethod = null then
      raise <| ExpressionGenerateError (SR.SOMA1024 (funcType.FullName, argType.FullName), loc, None)
    try
      let result = invokeMethod.Invoke(func, [| arg |])
      result, invokeMethod.ReturnType
    with 
    | exn ->
      raise <| ExpressionGenerateError (SR.SOMA1025 (funcType.FullName), loc, Some exn)

  let applyNewTuple (loc:Location) (values:(obj * Type) list)  =
    let elementValues = values |> Seq.map fst |> Seq.toArray
    let elementTypes = values |> Seq.map snd |> Seq.toArray
    let tupleType = FSharpType.MakeTupleType(elementTypes)
    FSharpValue.MakeTuple(elementValues, tupleType), tupleType

  let applyIn (loc:Location) (ident:string) (expr:obj, exprType:Type) =
    match expr with
    | :? IEnumerable as enumerable  ->
      let seq = enumerable |> Seq.cast<obj>
      box (ident, seq), typeof<Tuple<string, IEnumerable<obj>>>
    | _ -> 
      raise <| ExpressionGenerateError (SR.SOMA1022 (), loc, None)

  let generate expr (exprCtxt:IDictionary<string, obj * Type>) expression =
    let rec visitExpression = 
      function 
      | Factor factor -> visitFactor factor
      | Equal (lhs, rhs, loc) -> pair lhs rhs |> applyEqual loc
      | NotEqual (lhs, rhs, loc) -> pair lhs rhs |> applyNotEqual loc
      | LessThan (lhs, rhs, loc) -> pair lhs rhs |> applyLessThan loc
      | GreaterThan (lhs, rhs, loc) -> pair lhs rhs |> applyGreaterThan loc
      | LessThanOrEqual (lhs, rhs, loc) -> pair lhs rhs |> applyLessThanOrEqual loc
      | GreaterThanOrEqual (lhs, rhs, loc) -> pair lhs rhs |> applyGreaterThanOrEqual loc
      | AndAlso (lhs, rhs, loc) -> applyAndAlso loc (visitExpression lhs, lazy(visitExpression rhs))
      | OrElse (lhs, rhs, loc) -> applyOrElse loc (visitExpression lhs, lazy(visitExpression rhs))
      | Not (op, expression, loc) -> visitExpression expression |> applyNot op loc
      | Add (lhs, rhs, loc) -> pair lhs rhs |> applyAdd loc
      | Sub (lhs, rhs, loc) -> pair lhs rhs |> applySub loc
      | Mul (lhs, rhs, loc) -> pair lhs rhs |> applyMul loc
      | Div (lhs, rhs, loc) -> pair lhs rhs |> applyDiv loc
      | Mod (lhs, rhs, loc) -> pair lhs rhs |> applyMod loc
      | Application (expression, factor, loc) -> applyApplication loc (visitExpression expression) (visitFactor factor)
      | Tuple (factorList, loc) -> applyNewTuple loc (List.map visitFactor factorList)
      | In (factor, expression, loc) -> 
        let ident =
          match factor with
          | Var (name, _) -> name
          | _ -> raise <| ExpressionGenerateError (SR.SOMA1021 (), loc, None)
        let expr = visitExpression expression
        applyIn loc ident expr
    and visitFactor factor  =
      match factor with 
      | Var (name, loc) -> 
        match exprCtxt.TryGetValue name with 
        | true, (value, typ) -> 
          if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Nullable<_>> then
            value, typ
          else
            value, if value = null then typ else value.GetType()
        | _ -> raise <| ExpressionGenerateError (SR.SOMA1006 name, loc, None)
      | Property (expression, propName, loc) -> 
        let value, typ = visitFactor expression
        match (value, typ, propName) with
        | GetNullableProperty result -> result
        | GetOptionProperty result -> result
        | null, _, _ -> 
          raise <| ExpressionGenerateError (SR.SOMA1026 (propName, typ.FullName), loc, None)
        | GetDictValue result -> result
        | GetProperty result -> result
        | GetField result -> result
        | _ -> 
          raise <| ExpressionGenerateError (SR.SOMA1003 (propName, typ.FullName), loc, None)
      | StaticProperty (typeName, propName, loc) ->
        let typ = 
          match typeName with 
          | GetType typ -> typ 
          | _ -> raise <| ExpressionGenerateError (SR.SOMA1005(typeName), loc, None)
        match (typ, propName) with
        | GetStaticProperty result -> result
        | GetStaticField result -> result
        | _ ->  raise <| ExpressionGenerateError (SR.SOMA1004 (propName, typ.FullName), loc, None)
      | Parens expression-> visitExpression expression
      | Null -> null, typeof<obj> 
      | Unit -> box (()), typeof<obj>
      | Boolean v -> box v, typeof<Boolean>
      | Int32 v -> box v, typeof<Int32>
      | String v -> box v, typeof<String>
      | Double v -> box v, typeof<Double>
      | Single v -> box v, typeof<Single>
      | Int64 v -> box v, typeof<Int64>
      | UInt64 v -> box v, typeof<UInt64>
      | UInt32 v -> box v, typeof<UInt32>
      | Int16 v -> box v, typeof<Int16>
      | UInt16 v -> box v, typeof<UInt16>
      | SByte v -> box v, typeof<SByte>
      | Byte v -> box v, typeof<Byte>
      | Decimal v -> box v, typeof<Decimal>
    and pair lhs rhs = 
      let lhs = visitExpression lhs
      let rhs = visitExpression rhs
      lhs, rhs
    try
      visitExpression expression
    with
    | ExpressionGenerateError (message, loc, ex) ->
      let message = appendDetail (message) expr loc
      match ex with
      | Some ex ->
        raise <| ExpressionException (message, ex)
      | _ ->
        raise <| ExpressionException (message)

  let evaluate expr exprCtxt (parser:Func<string, Expression>) =
    parser.Invoke expr 
    |> generate expr exprCtxt