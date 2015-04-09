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
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

[<AutoOpen>]
module internal Util = 

  val inline (+>) : StringBuilder -> string -> StringBuilder
  
  val inline (+!) : StringBuilder -> string -> unit

  val asOption<'T when 'T : null and 'T : equality> : 'T -> 'T option

  type MaybeBuilder = 
    new : unit -> MaybeBuilder
    member Bind: 'a option * ('a -> 'b option) -> 'b option
    member Return: 'a -> 'a option
    member Zero: unit -> 'a option

  val maybe : MaybeBuilder

  val map<'T1, 'T2 when 'T1 : comparison> : ('T1 * 'T2) list  -> Map<'T1, 'T2>

  module Dict =
    val toSeq : seq<KeyValuePair<'TKey, 'TValue>> -> seq<'TKey * 'TValue>
    val toArray : IDictionary<'TKey, 'TValue> -> array<'TKey * 'TValue>
    val toList : IDictionary<'TKey, 'TValue> -> list<'TKey * 'TValue>

  module Map =
    val ofDict : seq<KeyValuePair<'TKey, 'TValue>> -> Map<'TKey, 'TValue>

  module Seq =
    val peek : seq<'T> -> seq<'T * bool>

  module Reflection =
    val getOptionInfo : Type -> UnionCaseInfo * UnionCaseInfo * Type
    val isOptionType : Type -> bool
    val makeOption : obj * Type -> obj
    val getOptionElement : obj * Type -> obj * Type
    val isNullableType : Type -> bool
    val makeNullable : obj * Type -> obj
    val getNullableElement : obj * Type -> obj * Type
    val isNumberType : Type -> bool
    val zero : Type -> obj
    val one : Type -> obj
    val incr : obj * Type -> obj
    val lessThan : obj * Type * int -> bool
    val isAnonymousType : Type -> bool
    val isGenericDictionary : typ:Type -> bool
    val getGenericDictionaryType : typ:Type -> Type
    val getGenericDictionaryValue : name:string * obj:obj * typ:Type -> obj option
    val changeTypeFromSeqToList : Type -> seq<obj> -> obj
    val changeTypeFromSeqToResizeArray : Type -> seq<obj>-> obj
