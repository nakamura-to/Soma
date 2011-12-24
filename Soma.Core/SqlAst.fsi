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
open Soma.Core.Text

/// <summary>The SQL abstract syntax tree.</summary>
module SqlAst =

  type Statement =
    | Statement of Node list
    | Set of string * Statement * Statement * Location

  and Node =
    | Select of string * Node list * Location
    | From of string * Node list * Location
    | Where of string * Node list * Location
    | Having of string * Node list * Location
    | GroupBy of string * Node list * Location
    | OrderBy of string * Node list * Location
    | ForUpdate of string * Node list * Location
    | And of string * Node list
    | Or of string * Node list
    | Parens of Statement
    | IfBlock of IfComment * ElifComment list * ElseComment option * Node list
    | ForBlock of ForComment * Node list
    | Literal of string
    | Whitespaces of string
    | Newline of string
    | Word of string
    | Other of string
    | BlockComment of string
    | LineComment of string
    | BindVarComment of string * Node * Location
    | BindVarsComment of string * Node * Location
    | EmbeddedVarComment of string * Location

  and IfComment =
    | IfComment of string * Node list * Location

  and ElifComment =
    | ElifComment of string * Node list * Location

  and ElseComment =
    | ElseComment of Node list * Location

  and ForComment =
    | ForComment of string * Node list * Location

  exception internal SqlParseError of Message * Location

  exception internal SqlParseErrorWithContext of obj

  module internal LexHelper =
    open Microsoft.FSharp.Text.Lexing
    val handleUnclosedExpressionComment : Position -> 'T
    val handleUnclosedBlockComment : Position -> 'T
    val handleUnclosedSingleQuote : Position -> 'T
    val handleUnknownExpressionDirecitive : Position -> string -> 'T

  module internal ParseHelper = 
    open Microsoft.FSharp.Text.Parsing
    val newStatement : IParseState -> Node list -> Statement
    val newSet : IParseState -> Statement -> string -> Statement -> Statement
    val newSelect : IParseState -> string -> Node list -> Node
    val newFrom : IParseState -> string -> Node list -> Node
    val newWhere : IParseState -> string -> Node list -> Node
    val newHaving : IParseState -> string -> Node list -> Node
    val newGroupBy : IParseState -> string -> Node list -> Node
    val newOrderBy : IParseState -> string -> Node list -> Node
    val newForUpdate : IParseState -> string -> Node list -> Node
    val newAnd : IParseState -> string -> Node list -> Node
    val newOr : IParseState -> string -> Node list -> Node
    val newParens : IParseState -> Statement -> Node
    val newIfBlock : IParseState -> IfComment -> ElifComment list -> ElseComment option -> Node list -> Node
    val handleIncompleteIfBlock : IParseState -> 'T
    val newForBlock : IParseState -> ForComment -> Node list -> Node
    val handleIncompleteForBlock : IParseState -> 'T
    val newWhitespaces : IParseState -> string -> Node
    val newNewline : IParseState -> string -> Node
    val newLiteral : IParseState -> string -> Node
    val newWord : IParseState -> string -> Node
    val newOther : IParseState -> string -> Node
    val newBlockComment : IParseState -> string -> Node
    val handleIncompleteBindVarComment : IParseState -> string -> 'T
    val newLineComment : IParseState -> string -> Node
    val newBindVarComment : IParseState -> string -> Node -> Node
    val newEmbeddedVarComment : IParseState -> string -> Node
    val newIfComment : IParseState -> string -> Node list -> IfComment
    val newElifComment : IParseState -> string -> Node list -> ElifComment
    val newElseComment : IParseState -> Node list -> ElseComment
    val newForComment : IParseState -> string -> Node list -> ForComment
    val parse_error_rich: (ParseErrorContext<'tok> -> unit) option
