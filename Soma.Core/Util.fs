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
open System.Text
open System.Reflection
open System.Runtime.CompilerServices;
open System.Collections
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

[<AutoOpen>]
module Util =
  
  let inline (+>) (buf:StringBuilder) (text:string) = buf.Append(text)
  
  let inline (+!) (buf:StringBuilder) (text:string) = buf.Append(text) |> ignore

  let asOption<'T when 'T : null and 'T : equality> (value:'T) =
    if value <> null then
      Some value
    else 
      None

  type MaybeBuilder() =
    member this.Bind (x, f) =
      match x with 
      | Some y -> f y
      | None -> None
    member this.Return (x) = 
      Some x
    member this.Zero () = 
      None
  
  let maybe = MaybeBuilder()

  let map list = Map.ofList list

  module Dict =

    let toSeq d = d |> Seq.map (function KeyValue(k,v) -> (k, v))
    
    let toArray (d:IDictionary<_, _>) = d |> toSeq |> Seq.toArray
    
    let toList (d:IDictionary<_, _>) = d |> toSeq |> Seq.toList

  module Map =

    let ofDict d = d |> Seq.map (function KeyValue(k, v) -> k, v) |> Map.ofSeq

  module Seq =

    let peek (source:seq<'T>) = seq {
      use ie = source.GetEnumerator() 
      if ie.MoveNext() then
        let iref = ref ie.Current
        while ie.MoveNext() do
          let j = ie.Current 
          yield (!iref, true)
          iref := j
        yield (!iref, false) }

  module Reflection =

    let rethrow ex =
      let m = typeof<Exception>.GetMethod("PrepForRemoting", BindingFlags.NonPublic ||| BindingFlags.Instance)
      m.Invoke(ex, [||]) |> ignore
      raise ex

    let getOptionInfo typ =
      let cases = FSharpType.GetUnionCases(typ)
      let none = FSharpType.GetUnionCases(typ).[0]
      let some = FSharpType.GetUnionCases(typ).[1]
      none, some, some.DeclaringType.GetGenericArguments().[0]

    let isOptionType (typ:Type) = 
      typ.IsGenericType
      &&
      not typ.IsGenericTypeDefinition
      && 
      typ.GetGenericTypeDefinition() = typedefof<option<_>>

    let makeOption (elementValue:obj, optionType) = 
      let none, some, elementType = getOptionInfo optionType
      if elementValue = null then
        FSharpValue.MakeUnion(none, [||])
      else
        let value = Convert.ChangeType(elementValue, elementType)
        FSharpValue.MakeUnion(some, [| value |])

    let getOptionElement (optionValue:obj, optionType:Type) =
      let none, some, elementType = getOptionInfo optionType
      let _, fields = FSharpValue.GetUnionFields(optionValue, optionType)
      let value = if fields.Length > 0 then fields.[0] else null
      value, elementType

    let isNullableType (typ:Type) =
      typ.IsGenericType
      &&
      not typ.IsGenericTypeDefinition
      &&
      typ.GetGenericTypeDefinition() = typedefof<Nullable<_>>
    
    let makeNullable (elementValue:obj, nullableType:Type) =
      if elementValue = null then
        null
      else
        let ctor = nullableType.GetConstructor([| elementValue.GetType() |])
        ctor.Invoke([| elementValue |])

    let getNullableElement (nullableValue:obj, nullableType:Type) =
      let hasValue _ =
        let prop = nullableType.GetProperty "HasValue"
        prop.GetValue(nullableValue, null) :?> bool
      let value _ =
        let prop = nullableType.GetProperty "Value"
        prop.GetValue(nullableValue, null)
      let underlyingType = Nullable.GetUnderlyingType nullableType
      if nullableValue <> null && hasValue () then
        value (), underlyingType
      else
        null, underlyingType

    let isNumberTypeCore typ =
      typ = typeof<int32> 
      || typ = typeof<int64>
      || typ = typeof<decimal>
      || typ = typeof<uint32>
      || typ = typeof<uint64>
      || typ = typeof<int16>
      || typ = typeof<uint16>
      || typ = typeof<single>
      || typ = typeof<double>
      || typ = typeof<byte>
      || typ = typeof<sbyte>
      || typ = typeof<bigint>
      || typ = typeof<nativeint>
      || typ = typeof<unativeint>

    let isNumberType typ =
      if isOptionType typ then
        let _, _, elementType = getOptionInfo typ
        isNumberTypeCore elementType
      elif isNullableType typ then
        isNumberTypeCore (Nullable.GetUnderlyingType(typ))
      else
        isNumberTypeCore typ

    let zeroCore typ =
      if typ = typeof<int32> then box 0
      elif typ = typeof<int64> then box 0L
      elif typ = typeof<decimal> then box 0M
      elif typ = typeof<uint32> then box 0u
      elif typ = typeof<uint64> then box 0UL
      elif typ = typeof<int16> then box 0s
      elif typ = typeof<uint16> then box 0us
      elif typ = typeof<single> then box 0.f
      elif typ = typeof<double> then box 0.
      elif typ = typeof<byte> then box 0uy
      elif typ = typeof<sbyte> then box 0y
      elif typ = typeof<bigint> then box 0I
      elif typ = typeof<nativeint> then box 0n
      elif typ = typeof<unativeint> then box 0un
      else invalidArg "typ" ("unsupported " + (string typ))

    let zero typ =
      if isOptionType typ then
        let _, _, elementType = getOptionInfo typ
        let elementValue = zeroCore elementType
        makeOption (elementValue, typ)
      elif isNullableType typ then
        let elementValue = zeroCore (Nullable.GetUnderlyingType typ)
        makeNullable (elementValue, typ)
      else
        zeroCore typ

    let oneCore typ =
      if typ = typeof<int32> then box 1
      elif typ = typeof<int64> then box 1L
      elif typ = typeof<decimal> then box 1M
      elif typ = typeof<uint32> then box 1u
      elif typ = typeof<uint64> then box 1UL
      elif typ = typeof<int16> then box 1s
      elif typ = typeof<uint16> then box 1us
      elif typ = typeof<single> then box 1.f
      elif typ = typeof<double> then box 1.
      elif typ = typeof<byte> then box 1uy
      elif typ = typeof<sbyte> then box 1y
      elif typ = typeof<bigint> then box 1I
      elif typ = typeof<nativeint> then box 1n
      elif typ = typeof<unativeint> then box 1un
      else invalidArg "typ" ("unsupported " + (string typ))

    let one typ =
      if isOptionType typ then
        let _, _, elementType = getOptionInfo typ
        let elementValue = oneCore elementType
        makeOption (elementValue, typ)
      elif isNullableType typ then
        let elementValue = oneCore (Nullable.GetUnderlyingType typ)
        makeNullable (elementValue, typ)
      else
        oneCore typ

    let incrCore (value:obj, typ:Type) =
      match value with
      | :? int32 as v -> box (v + 1)
      | :? int64 as v -> box (v + 1L)
      | :? decimal as v -> box (v + 1M)
      | :? uint32 as v -> box (v + 1u)
      | :? uint64 as v -> box (v + 1UL)
      | :? int16 as v -> box (v + 1s)
      | :? uint16 as v -> box (v + 1us)
      | :? single as v -> box (v + 1.f)
      | :? double as v -> box (v + 1.)
      | :? byte as v -> box (v + 1uy)
      | :? sbyte as v -> box (v + 1y)
      | :? bigint as v -> box (v + 1I)
      | :? nativeint as v -> box (v + 1n)
      | :? unativeint as v -> box (v + 1un)
      | _ -> invalidArg "obj" (value.ToString())

    let incr (value:obj, typ) =
      if isOptionType typ then
        let elementValue, elementType = getOptionElement (value, typ)
        let elementValue =
          if elementValue = null then
            one elementType
          else 
            incrCore (elementValue, elementType)
        makeOption (elementValue, typ)
      elif isNullableType typ then
        let elementValue, elementType = getNullableElement (value, typ)
        let elementValue =
          if elementValue = null then
            one elementType
          else 
            incrCore (elementValue, elementType)
        makeNullable (elementValue, typ)
      else
        incrCore (value, typ)

    let lessThanCore (lhs:obj, typ:Type, rhs:int) =
      match lhs with
      | :? int32 as x -> x < (int32 rhs)
      | :? int64 as x -> x < (int64 rhs)
      | :? decimal as x -> x < (decimal rhs)
      | :? uint32 as x -> x < (uint32 rhs)
      | :? uint64 as x -> x < (uint64 rhs)
      | :? int16 as x -> x < (int16 rhs)
      | :? uint16 as x -> x < (uint16 rhs)
      | :? single as x -> x < (single rhs)
      | :? double as x -> x < (double rhs)
      | :? byte as x -> x < (byte rhs)
      | :? sbyte as x -> x < (sbyte rhs)
      | :? bigint as x -> x < (bigint rhs)
      | :? nativeint as x -> x < (nativeint rhs)
      | :? unativeint as x -> x < (unativeint rhs)
      | _ -> false

    let lessThan (lhs:obj, typ, rhs) =
      if isOptionType typ then
        let elementValue, elementType = getOptionElement (lhs, typ)
        if elementValue = null then true
        else lessThanCore (elementValue, elementType, rhs)
      elif isNullableType typ then
        let elementValue, elementType = getNullableElement (lhs, typ)
        if elementValue = null then true
        else lessThanCore (elementValue, elementType, rhs)
      else
        lessThanCore (lhs, typ, rhs)

    let isAnonymousType (typ:Type) =
      let isGenerated () = Attribute.IsDefined(typ, typeof<CompilerGeneratedAttribute>, false)
      let hasValidName () = typ.Name.Contains("AnonymousType") && (typ.Name.StartsWith("<>") || typ.Name.StartsWith("VB$"))
      isGenerated () && hasValidName ()

    let getGenericDictionaryType (typ:Type) =
      if typ.IsInterface && typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<IDictionary<_, _>> then
        typ
      else 
        typ.GetInterface "System.Collections.Generic.IDictionary`2"

    let isGenericDictionary (typ:Type) =
      (getGenericDictionaryType typ) <> null

    let getGenericDictionaryValue (name:string, obj:obj, typ:Type) =
      let typ = getGenericDictionaryType typ
      let m = typ.GetMethod("TryGetValue")
      let args = [| box name; null |]
      match m.Invoke(obj, args) with
      | :? bool as b when b -> Some (args.[1])
      | _ -> None

    type ToList =
      static member Invoke<'T>(seq:seq<'T>) =
        use e = seq.GetEnumerator()
        let mutable res = [] 
        while e.MoveNext() do
            res <- e.Current :: res
        List.rev res

    type ToResizeArray =
      static member Invoke<'T>(seq:seq<'T>) =
        ResizeArray (seq)

    let changeTypeFromSeq (seq:seq<obj>) (elementTyp:Type) (helperType:Type) =
      let m = typeof<System.Linq.Enumerable>.GetMethod("Cast")
      let m = m.MakeGenericMethod elementTyp
      let seq = m.Invoke(null, [| seq |])
      let m = helperType.GetMethod("Invoke", BindingFlags.NonPublic ||| BindingFlags.Static)
      let m = m.MakeGenericMethod (elementTyp)
      try
        m.Invoke(null, [| seq |])
      with
        | :? TargetInvocationException as e -> rethrow e.InnerException

    let changeTypeFromSeqToList (elementTyp:Type) (seq:seq<obj>) =
      changeTypeFromSeq seq elementTyp typeof<ToList>

    let changeTypeFromSeqToResizeArray (elementTyp:Type) (seq:seq<obj>) =
      changeTypeFromSeq seq elementTyp typeof<ToResizeArray>


