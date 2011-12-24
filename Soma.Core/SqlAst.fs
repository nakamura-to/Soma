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

  exception SqlParseError of Message * Location

  exception SqlParseErrorWithContext of obj

  module LexHelper =
    open Microsoft.FSharp.Text.Lexing

    let getLocation (position : Position) =
      { Location.pos_fname = position.pos_fname 
        pos_lnum = position.pos_lnum
        pos_bol = position.pos_bol
        pos_cnum = position.pos_cnum }

    let handleUnclosedExpressionComment position =
      let loc = getLocation position
      raise <| SqlParseError (SR.SOMA2010 (), loc)

    let handleUnclosedBlockComment position = 
      let loc = getLocation position
      raise <| SqlParseError (SR.SOMA2011 (), loc)

    let handleUnclosedSingleQuote position =
      let loc = getLocation position
      raise <| SqlParseError (SR.SOMA2012 (), loc)

    let handleUnknownExpressionDirecitive position (directive:string) =
      let loc = getLocation position
      raise <| SqlParseError (SR.SOMA2019 directive, loc)

  module ParseHelper = 
    open Microsoft.FSharp.Text.Parsing

    let getLocation (parseState : IParseState) index =
      let p = parseState.InputStartPosition index
      { Location.pos_fname = p.pos_fname 
        pos_lnum = p.pos_lnum
        pos_bol = p.pos_bol
        pos_cnum = p.pos_cnum }

    let newStatement (parseState : IParseState) nodes =
      Statement (List.rev nodes)

    let newSet parseState lhs set rhs =
      Set (set, lhs, rhs, getLocation parseState 2)

    let newSelect parseState select nodes =
      Select (select, List.rev nodes, getLocation parseState 1)

    let newFrom parseState from nodes =
      From (from, List.rev nodes, getLocation parseState 1)

    let newWhere parseState where nodes =
      Where (where, List.rev nodes, getLocation parseState 1)

    let newHaving parseState having nodes =
      Having (having, List.rev nodes, getLocation parseState 1)

    let newGroupBy parseState groupBy nodes =
      GroupBy (groupBy, List.rev nodes, getLocation parseState 1)

    let newOrderBy parseState orderBy nodes =
      OrderBy (orderBy, List.rev nodes, getLocation parseState 1)

    let newForUpdate parseState forUpdate nodes =
      ForUpdate (forUpdate, List.rev nodes, getLocation parseState 1)

    let newAnd (parseState : IParseState) ``and`` nodes =
      And (``and``, List.rev nodes)

    let newOr (parseState : IParseState) ``or`` nodes =
      Or (``or``, List.rev nodes)

    let newParens (parseState : IParseState) statement =
      Parens (statement)

    let newIfBlock (parseState : IParseState) ifComment elifComments elseComment nodes =
      IfBlock (ifComment, List.rev elifComments, elseComment, List.rev nodes)

    let handleIncompleteIfBlock (parseState : IParseState) =
      let loc = getLocation parseState 1
      raise <| SqlParseError (SR.SOMA2005 (), loc)

    let newForBlock (parseState : IParseState) forComment nodes =
      ForBlock (forComment, List.rev nodes)

    let handleIncompleteForBlock (parseState : IParseState) =
      let loc = getLocation parseState 1
      raise <| SqlParseError (SR.SOMA2013 (), loc)

    let newWhitespaces (parseState : IParseState) whitespaces =
      Whitespaces (whitespaces)

    let newNewline (parseState : IParseState) newline =
      Newline (newline)

    let newLiteral (parseState : IParseState) literal =
      Literal (literal)

    let newWord (parseState : IParseState) word =
      Word (word)

    let newOther (parseState : IParseState) other =
      Other (other)

    let newBlockComment (parseState : IParseState) blockComment =
      BlockComment (blockComment)

    let newLineComment (parseState : IParseState) lineComment =
      LineComment (lineComment)

    let newBindVarComment parseState (bindVarComment:string) node =
      let bindVarComment = bindVarComment.Trim()
      match node with 
      | Literal _ -> 
        BindVarComment (bindVarComment, node, (getLocation parseState 1)) 
      | Parens _ -> 
        BindVarsComment (bindVarComment, node, (getLocation parseState 1)) 
      | _ -> 
        let loc = getLocation parseState 1        
        raise <| SqlParseError (SR.SOMA2008 bindVarComment, loc)

    let handleIncompleteBindVarComment (parseState : IParseState) (bindVarComment:string) =
      let loc = getLocation parseState 1
      raise <| SqlParseError (SR.SOMA2009 bindVarComment, loc)

    let newEmbeddedVarComment parseState (embeddedVarComment:string) =
      EmbeddedVarComment (embeddedVarComment.Trim(), (getLocation parseState 1)) 

    let newIfComment (parseState : IParseState) (ifComment:string) nodes =
      IfComment (ifComment.Trim(), List.rev nodes, (getLocation parseState 1)) 

    let newElifComment parseState (elifComment:string) nodes =
      ElifComment (elifComment.Trim(), List.rev nodes, (getLocation parseState 1)) 

    let newElseComment parseState nodes =
      ElseComment (List.rev nodes, (getLocation parseState 1))  

    let newForComment parseState (forComment:string) nodes =
      ForComment (forComment.Trim(), List.rev nodes, (getLocation parseState 1)) 

    let raiseError (ctxt:ParseErrorContext<_>) =
      raise <| SqlParseErrorWithContext (box ctxt) |> ignore

    let parse_error_rich = Some(raiseError)