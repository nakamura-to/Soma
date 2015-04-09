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
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open Soma.Core.Text

/// <summary>The expression abstract syntax tree.</summary>
module ExpressionAst =

  type Expression = 
    | Factor of Factor
    | Add of Expression * Expression * Location
    | Sub of Expression * Expression * Location
    | Mul of Expression * Expression * Location
    | Div of Expression * Expression * Location
    | Mod of Expression * Expression * Location
    | Equal of Expression * Expression * Location
    | NotEqual of Expression * Expression * Location
    | LessThan of Expression * Expression * Location
    | GreaterThan of Expression * Expression * Location
    | LessThanOrEqual of Expression * Expression * Location
    | GreaterThanOrEqual of Expression * Expression * Location
    | AndAlso of Expression * Expression * Location
    | OrElse of Expression * Expression * Location
    | Not of string * Expression * Location
    | Application of Expression * Factor * Location
    | Tuple of Factor list * Location
    | In of Factor * Expression * Location

  and Factor =
    | Null
    | Unit
    | Boolean of bool
    | Byte of byte
    | SByte of sbyte 
    | Int16 of int16
    | UInt16 of uint16 
    | Int32 of int32 
    | UInt32 of uint32 
    | Int64 of int64 
    | UInt64 of uint64 
    | Single of single 
    | Double of double 
    | Decimal of decimal 
    | String of string
    | Var of string * Location
    | Parens of Expression
    | Property of Factor * string * Location
    | StaticProperty of string * string * Location

  exception internal ExpressionParseError of Message * Location

  exception internal ExpressionParseErrorWithContext of obj

  module internal LexHelper = 

    val handleUnclosedSingleQuote : Position -> 'T
    val handleUnclosedSquareBranket : Position -> 'T
    val handleUnclosedDollarMark : Position -> 'T
    val handleUnsupportedToken : Position -> string -> 'T

  module internal ParseHelper = 

    val newFactor : IParseState -> Factor -> Expression
    val newAdd : IParseState -> Expression -> Expression -> Expression
    val newSub : IParseState -> Expression -> Expression -> Expression
    val newMul : IParseState -> Expression -> Expression -> Expression
    val newDiv : IParseState -> Expression -> Expression -> Expression
    val newMod : IParseState -> Expression -> Expression -> Expression
    val newEqual : IParseState -> Expression -> Expression -> Expression
    val newNotEqual : IParseState -> Expression -> Expression -> Expression
    val newLessThan : IParseState -> Expression -> Expression -> Expression
    val newGreaterThan : IParseState -> Expression -> Expression -> Expression
    val newLessThanOrEqual : IParseState -> Expression -> Expression -> Expression
    val newGreaterThanOrEqual : IParseState -> Expression -> Expression -> Expression
    val newAndAlso : IParseState -> Expression -> Expression -> Expression
    val newOrElse : IParseState -> Expression -> Expression -> Expression
    val newNot : IParseState -> string -> Expression -> Expression
    val newApplication : IParseState -> Expression -> Factor -> Expression
    val newTuple : IParseState -> Factor list -> Expression
    val newIn : IParseState -> Factor -> Expression -> Expression
    val newNull : IParseState -> Factor
    val newBoolean : IParseState -> bool -> Factor
    val newByte : IParseState -> string -> Factor
    val newSByte : IParseState -> string -> Factor
    val newInt16 : IParseState -> string -> Factor
    val newUInt16 : IParseState -> string -> Factor
    val newInt32 : IParseState -> string -> Factor
    val newUInt32 : IParseState -> string -> Factor
    val newInt64 : IParseState -> string -> Factor
    val newUInt64 : IParseState -> string -> Factor
    val newSingle : IParseState -> string -> Factor
    val newDouble : IParseState -> string -> Factor
    val newDecimal : IParseState -> string -> Factor
    val newString : IParseState -> string -> Factor
    val newVar : IParseState -> string -> Factor
    val newUnit : IParseState -> Factor
    val newParens: IParseState -> Expression -> Factor
    val newProperty : IParseState -> Factor -> string -> Factor
    val newStaticProperty : IParseState -> string -> string -> Factor
    val parse_error_rich: (ParseErrorContext<_> -> unit) option