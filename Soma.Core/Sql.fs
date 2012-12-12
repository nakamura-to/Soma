//----------------------------------------------------------------------------
//
// Copyright (c) 2011 The Soma Team. 
//
// This source code is subject to terms and exprCtxts of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.txt file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

namespace Soma.Core

open System
open System.Collections
open System.Collections.Generic
open System.Data
open System.Data.Common
open System.Globalization
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open Soma.Core
open Soma.Core.SqlAst
open Soma.Core.Text
open Soma.Core.SqlParser

exception NoInsertablePropertyException of unit with
  override this.Message =
    match this :> exn with
    | NoInsertablePropertyException() -> 
      let message = SR.SOMA4024 ()
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

exception NoUpdatablePropertyException of unit with
  override this.Message =
    match this :> exn with
    | NoUpdatablePropertyException() -> 
      let message = SR.SOMA4025 ()
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

[<Class>]
[<Sealed>]
[<AllowNullLiteral>]
type CharString(value:string) = 
  static let eq (a:obj) (b:obj) =
    if obj.ReferenceEquals(a, b) then
      true
    else 
      match a, b with
      | (:? CharString as a), (:? CharString as b) -> a.Value = b.Value
      | _ -> false
  member this.Value = if value = null then String.Empty else value
  override this.Equals(other) = eq this other
  override this.GetHashCode() = this.Value.GetHashCode()
  override this.ToString() = this.Value
  static member op_Equality(a:CharString, b:CharString) = eq a b
  static member op_Inequality(a:CharString, b:CharString) = not <| eq a b

  interface IComparable<CharString> with
    member this.CompareTo(other) = 
      if eq this other then 0
      else 
        match other with
        | null -> 1
        | _ -> this.Value.CompareTo(other.Value)

type InsertOpt() =
  let mutable exclude:seq<string> = null
  let mutable ``include``:seq<string> = null
  let mutable excludeNull:bool = false
  member this.Exclude 
    with get () = if exclude = null then Seq.empty else exclude
    and  set (v) = exclude <- v
  member this.Include 
    with get () = if ``include`` = null then Seq.empty else ``include``
    and  set (v) = ``include`` <- v
  member this.ExcludeNull
    with get () = excludeNull
    and  set (v) = excludeNull <- v

type UpdateOpt() =
  let mutable exclude:seq<string> = null
  let mutable ``include``:seq<string> = null
  let mutable excludeNull:bool = false
  let mutable ignoreVersion:bool = false
  member this.Exclude 
    with get () = if exclude = null then Seq.empty else exclude
    and  set (v) = exclude <- v
  member this.Include 
    with get () = if ``include`` = null then Seq.empty else ``include``
    and  set (v) = ``include`` <- v
  member this.ExcludeNull
    with get () = excludeNull
    and  set (v) = excludeNull <- v
  member this.IgnoreVersion
    with get () = ignoreVersion
    and  set (v) = ignoreVersion <- v

type DeleteOpt() =
  let mutable ignoreVersion:bool = false
  member this.IgnoreVersion
    with get () = ignoreVersion
    and  set (v) = ignoreVersion <- v

type SqlBuilder(dialect:IDialect, ?capacity, ?parameterNameSuffix) =

  let sql = StringBuilder(defaultArg capacity 200)
  let formattedSql = StringBuilder(defaultArg capacity 200)
  let parameters = ResizeArray<PreparedParameter>()
  let parameterNameSuffix = defaultArg parameterNameSuffix String.Empty
  let mutable parameterIndex = 0

  member this.Sql = sql

  member this.FormattedSql = formattedSql

  member this.Parameters = parameters

  member this.ParameterIndex
    with get () = parameterIndex
    and  set (v) = parameterIndex <- v

  member this.Append (fragment : string) =
    sql.Append fragment |> ignore
    formattedSql.Append fragment |> ignore

  member this.Append (fragment : StringBuilder) =
    sql.Append fragment |> ignore
    formattedSql.Append fragment |> ignore

  member this.CutBack (size) =
    sql.Remove(sql.Length - size, size) |> ignore
    formattedSql.Remove(formattedSql.Length - size, size) |> ignore

  member this.Bind (value : obj, typ : Type) =
    let value, typ, dbType = dialect.ConvertFromClrToDb(value, typ, null)
    let parameterName = dialect.CreateParameterName(parameterIndex) + parameterNameSuffix
    parameterIndex <- parameterIndex + 1
    sql.Append(parameterName) |> ignore
    formattedSql.Append (dialect.FormatAsSqlLiteral(value, typ, dbType)) |> ignore
    parameters.Add(
      { Name = parameterName
        Value = value
        Type = typ
        DbType = dbType
        Direction = Direction.Input
        Size = None
        Precision = None
        Scale = None
        UdtTypeName = null })
  member this.Build () =
    { Text = sql.ToString().Trim()
      FormattedText = formattedSql.ToString().Trim()
      Parameters = List.ofSeq parameters }

type SqlException (message:Message, ?innerException:exn) =
  inherit InvalidOperationException (message.Format (), match innerException with Some ex -> ex | _ -> null)
  member this.MessageId = message.Id

module RewriteHelper =
  let writeIfComment ifComment (buf:StringBuilder) f = 
    match ifComment with
    | IfComment(expression, nodeList, _) ->
      buf.Append "/*% if " |> ignore
      buf.Append expression |> ignore
      buf.Append " */" |> ignore
      List.fold f buf nodeList

  let writeElifComment elifComment (buf:StringBuilder) f = 
    match elifComment with
    | ElifComment(expression, nodeList, _) ->
      buf.Append "/*% elif " |> ignore
      buf.Append expression |> ignore
      buf.Append " */" |> ignore
      List.fold f buf nodeList

  let writeElseComment elseComment (buf:StringBuilder) f = 
    match elseComment with
    | ElseComment(nodeList, _) ->
      buf.Append "/*% else */" |> ignore
      List.fold f buf nodeList
  
  let writeForComment forComment (buf:StringBuilder) f  = 
    match forComment with
    | ForComment(expression, nodeList, _) ->
      buf.Append "/*% for " |> ignore
      buf.Append expression |> ignore
      buf.Append " */" |> ignore
      List.fold f buf nodeList

  let writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) (buf:StringBuilder) f  = 
    let buf = writeIfComment ifComment buf f
    let buf = List.fold (fun buf comment -> writeElifComment comment buf f) buf elifCommentList
    let buf =
      match elseComment with 
      | Some comment -> writeElseComment comment buf f
      | _ -> buf
    buf.Append "/*% end */" |> ignore
    List.fold f buf nodeList

  let writeForBlock (forComment, nodeList) (buf:StringBuilder) f =
    let (buf:StringBuilder) = writeForComment forComment buf f
    buf.Append "/*% end */" |> ignore
    List.fold f buf nodeList

  let writeBindVarComment (expression:string, node) (buf:StringBuilder) f = 
    buf.Append "/* " |> ignore
    buf.Append expression |> ignore
    buf.Append " */" |> ignore
    f buf node

  let writeEmbeddedVarComment (expression:string) (buf:StringBuilder) = 
    buf.Append "/*# " |> ignore
    buf.Append expression |> ignore
    buf.Append " */" |> ignore
    buf

  let writeParens statement (level:int ref) (buf:StringBuilder) f = 
    buf.Append "(" |> ignore
    incr level
    let (buf:StringBuilder) = f buf statement
    decr level
    buf.Append ")"

  let writeSet (set:string, lhs, rhs) (buf:StringBuilder) f = 
    let buf:StringBuilder = f buf lhs
    buf.Append set |> ignore
    f buf rhs

open RewriteHelper

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sql =

  type State (dialect:IDialect, exprCtxt, parameterIndex) =
  
    let buf = SqlBuilder(dialect, ParameterIndex = parameterIndex)
    let mutable exprCtxt:IDictionary<string, obj * Type> = exprCtxt
    let mutable isAvailable = false 
    let mutable startsWithClause = false
    let mutable isInsideBlock = false

    member this.Sql = buf.Sql

    member this.FormattedSql = buf.FormattedSql

    member this.Parameters = buf.Parameters

    member this.ParameterIndex = buf.ParameterIndex

    member this.ExprCtxt 
      with get () = exprCtxt 
      and  set (v) = exprCtxt <- v

    member this.IsAvailable 
      with get () = isAvailable 
      and  set (v) = isAvailable <- v

    member this.IsInsideBlock 
      with get () = isInsideBlock 
      and  set (v) = isInsideBlock <- v

    member this.StartsWithClause = startsWithClause

    member this.AppendState (other : State) =
      buf.Sql.Append(other.Sql) |> ignore
      buf.FormattedSql.Append(other.FormattedSql) |> ignore
      other.Parameters |> Seq.iter (fun p -> buf.Parameters.Add(p))
      buf.ParameterIndex <- other.ParameterIndex

    member this.AppendFragment (fragment : string) =
      buf.Append(fragment)

    member this.AppendFragment (fragment : string, hint : Node) =
      if not startsWithClause && buf.Sql.ToString().Trim().Length = 0 then
        match hint with
        | Select _ | From _ | Where _ | Having _ | GroupBy _ | OrderBy _ | ForUpdate _ -> startsWithClause <- true
        | _ -> ()
      buf.Append(fragment)

    member this.Bind (value, typ) =
      buf.Bind(value, typ)

    member this.Build() =
      buf.Build()

  let startLocation = { pos_bol = 0; pos_fname = "Sql"; pos_cnum = 0; pos_lnum = 1 }

  let getErrorMessage (ctxt:ParseErrorContext<token>) =
    match ctxt.CurrentToken with
    | None -> 
      SR.SOMA2000 ctxt.Message
    | Some token -> 
      SR.SOMA2004 (token_to_string token) 

  let getLocation (lexbuf:LexBuffer<char>) =
    let pos = lexbuf.StartPos
    { Location.pos_bol = pos.pos_bol; pos_fname= pos.pos_fname; pos_cnum = pos.pos_cnum; pos_lnum = pos.pos_lnum }

  let appendDetail (message:Message) (sql:string) loc = 
      { message with Text = message.Text + " " + (SR.SOMA2006 (sql, loc.pos_lnum, loc.pos_cnum)).Text }

  let appendSimpleDetail (message:Message) (sql:string) = 
      { message with Text = message.Text + " " + (SR.SOMA2018 (sql)).Text }

  [<CompiledName("Parse")>]
  let parse sql =
    let lexbuf = LexBuffer<char>.FromString(sql)
    let loc = startLocation
    lexbuf.EndPos <- { pos_bol = loc.pos_bol; pos_fname = loc.pos_fname; pos_cnum = loc.pos_cnum; pos_lnum = loc.pos_lnum } 
    try
      SqlParser.start SqlLexer.tokenize lexbuf
    with 
    | SqlParseError (message, loc) -> 
      let message = appendDetail message sql loc
      raise <| SqlException (message)
    | SqlParseErrorWithContext obj ->
      let ctxt = obj :?> ParseErrorContext<token>
      let message = getErrorMessage ctxt
      let loc = getLocation lexbuf
      let message = appendDetail message sql loc
      raise <| SqlException (message) 
    | ex -> 
      let p = lexbuf.EndPos
      let loc = { Location.pos_bol = p.pos_bol; pos_fname= p.pos_fname; pos_cnum = p.pos_cnum; pos_lnum = p.pos_lnum }
      let message = appendDetail (SR.SOMA2001 ()) sql loc
      raise <| SqlException (message, ex)

  let concatExprCtxt (x:IDictionary<string, obj * Type>) (y:IDictionary<string, obj * Type>) = 
    let context = new Dictionary<string, obj * Type>(x) :> IDictionary<string, obj * Type>
    y |> Seq.iter (fun (KeyValue(key, value)) ->
      context.Remove(key) |> ignore
      context.Add(key, value) )
    context
  
  let (|If|_|) ifComment eval _ = 
    match ifComment with 
    | IfComment(expr, nodes, loc) -> 
      match eval expr loc with 
      | true -> Some nodes 
      | _ -> None 

  let (|Elif|_|) elifCommentList eval _ =
    List.tryPick (function 
      | ElifComment(expr, nodes, loc) -> 
        match eval expr loc with 
        | true -> Some nodes
        | _ -> None) elifCommentList
  
  let (|Else|_|) elseComment _ =
    match elseComment with 
    | Some (ElseComment (nodes, _)) -> Some nodes 
    | _ -> None

  let eval (config:IDbConfig) exprCtxt expr loc sql =
    try
      Expression.evaluate expr exprCtxt config.ExpressionParser
    with 
    | :? ExpressionException as ex ->
      let message = appendDetail (SR.SOMA2007 ()) sql loc
      let message = message.AppendText(ex.Message)
      match ex.InnerException with
      | null -> raise <| SqlException (message)
      | _ -> raise <| SqlException (message, ex)

  let invalidBlock keyword loc sql =
    let message = appendDetail (SR.SOMA2015 (keyword)) sql loc
    raise <| SqlException (message)

  let (|EmptyStatement|_|) =
    function 
    | Statement(nodes) ->
      match nodes with
      | [] -> Some ""
      | [Whitespaces(s)] -> Some s
      | _ -> None
    |_ -> None

  let generate config sql exprCtxt statement =
    let eval exprCtxt expr loc =
      eval config exprCtxt expr loc sql
    let rec foldInsideBlock (state : State) nodeList = 
      let preserve = state.IsInsideBlock
      state.IsInsideBlock <- true
      let state = List.fold visitNode state nodeList
      state.IsInsideBlock <- preserve
      state
    and visitStatement state = 
      function 
      | Statement nodeList -> 
        List.fold visitNode state nodeList 
      | Set (set, lhs, rhs, _) ->
        let state = visitStatement state lhs
        state.AppendFragment(set)
        let state = visitStatement state rhs
        state
    and visitNode (state : State) =
      function
      | Word(fragment)
      | Other(fragment)
      | Literal(fragment) ->
        state.IsAvailable <- true
        state.AppendFragment fragment
        state
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        state.AppendFragment fragment
        state
      | Select(keyword, nodeList, loc)
      | From(keyword, nodeList, loc) as node ->
        if state.IsInsideBlock then 
          invalidBlock keyword loc sql
        state.AppendFragment(keyword, node)
        List.fold visitNode state nodeList
      | Where(keyword, nodeList, loc)
      | GroupBy(keyword, nodeList, loc)
      | Having(keyword, nodeList, loc)
      | OrderBy(keyword, nodeList, loc)
      | ForUpdate(keyword, nodeList, loc) as node->
        if state.IsInsideBlock then 
          invalidBlock keyword loc sql
        let childState = List.fold visitNode (State(config.Dialect, state.ExprCtxt, state.ParameterIndex)) nodeList
        if childState.IsAvailable then 
          state.AppendFragment(keyword, node)
          state.AppendState(childState)
        elif childState.StartsWithClause then 
          state.IsAvailable <- true
          state.AppendState(childState)
        state
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        if state.IsAvailable then 
          state.AppendFragment(keyword)
        List.fold visitNode state nodeList
      | Parens(statement) ->
        match statement with
        | EmptyStatement(s) -> 
          state.IsAvailable <- true
          state.AppendFragment "("
          state.AppendFragment s
          state.AppendFragment ")"
        | _ -> 
          let childState = visitStatement (State(config.Dialect, state.ExprCtxt, state.ParameterIndex)) statement
          if childState.IsAvailable || childState.StartsWithClause then 
            state.IsAvailable <- true
            state.AppendFragment "("
            state.AppendState childState
            state.AppendFragment ")"
        state
      | BindVarComment(expr, _, loc) ->
        state.IsAvailable <- true ;
        state.Bind (eval state.ExprCtxt expr loc)
        state
      | BindVarsComment(expr, _, loc) ->
        let result, typ = eval state.ExprCtxt expr loc
        match result with 
        | :? IEnumerable as seq -> 
          let parameters = seq |> Seq.cast<obj> |> Seq.toList
          if not parameters.IsEmpty then
            state.AppendFragment "("
            state.Bind(parameters.Head, (parameters.Head.GetType()))
            Seq.iter (fun p -> 
              state.AppendFragment ", "
              state.Bind (p, p.GetType())) parameters.Tail
            state.AppendFragment(")")
        | param -> 
          state.Bind (param, typ)
        state.IsAvailable <- true
        state
      | EmbeddedVarComment(expr, loc) -> 
        let value, _ = eval state.ExprCtxt expr loc
        let fragment = string value
        if not <| String.IsNullOrWhiteSpace fragment then
          state.IsAvailable <- true
          state.AppendFragment fragment
        state
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        let eval expr loc  = 
          let result, _ = eval state.ExprCtxt expr loc
          true.Equals(result)
        let state =
          match () with 
          | If ifComment eval nodeList 
          | Elif elifCommentList eval nodeList 
          | Else elseComment nodeList -> foldInsideBlock state nodeList
          | _ -> state
        List.fold visitNode state nodeList
      | ForBlock(forComment, nodeList) -> 
        let preservedCtxt = state.ExprCtxt
        let state = 
          match forComment with 
          | ForComment (expr, nodeList, loc) ->
            let obj, typ = eval preservedCtxt expr loc
            match obj with
            | :? Tuple<string, seq<obj>> as tuple when typ = typeof<Tuple<string, seq<obj>>> ->
              let ident = tuple.Item1
              let seq = tuple.Item2
              let currentCtxt = new Dictionary<string, obj * Type>(preservedCtxt)
              let hasNextKey = ident + "_has_next"
              currentCtxt.Remove(ident) |> ignore
              currentCtxt.Remove(hasNextKey) |> ignore
              state.ExprCtxt <- currentCtxt
              seq
              |> Seq.peek
              |> Seq.map (fun (element, hasNext) -> 
                currentCtxt.Remove(ident) |> ignore
                currentCtxt.Add(ident, (element, typeof<obj>)) 
                currentCtxt.Remove(hasNextKey) |> ignore
                currentCtxt.Add(hasNextKey, (box hasNext, typeof<bool>)) 
                () )
              |> Seq.fold (fun (state:State) _ -> foldInsideBlock state nodeList) state
            | _ -> 
              let message = appendDetail (SR.SOMA2014 (expr, typ.FullName)) sql loc
              raise <| SqlException (message)
        state.ExprCtxt <- preservedCtxt
        List.fold visitNode state nodeList
    let state = visitStatement (State(config.Dialect, exprCtxt, 0)) statement
    state.Build()

  let resolveOrderByEmbeddedVariables config statement sql exprCtxt =
    let level = ref 0
    let insideOrderBy = ref false
    let rec visitStatement (buf:StringBuilder) = 
      function
      | Statement nodeList -> 
        List.fold visitNode buf nodeList
      | Set(set, lhs, rhs, _) ->
        writeSet (set, lhs, rhs) buf visitStatement
    and visitNode (buf:StringBuilder) =
      function
      | Word(fragment)
      | Other(fragment)
      | Literal(fragment) 
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        buf.Append fragment
      | Select(keyword, nodeList, _)
      | From(keyword, nodeList, _) 
      | Where(keyword, nodeList, _)
      | GroupBy(keyword, nodeList, _)
      | Having(keyword, nodeList, _)
      | ForUpdate(keyword, nodeList, _)
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        buf.Append keyword |> ignore
        List.fold visitNode buf nodeList
      | OrderBy(keyword, nodeList, _) -> 
        buf.Append keyword |> ignore
        insideOrderBy := true
        let buf = List.fold visitNode buf nodeList
        insideOrderBy := false
        buf
      | Parens(statement) ->
        writeParens statement level buf visitStatement
      | BindVarComment(expression, node, _)
      | BindVarsComment(expression, node, _) ->
        writeBindVarComment (expression, node) buf visitNode
      | EmbeddedVarComment(expression, loc) ->
        if !insideOrderBy then
          let evalResult = eval config exprCtxt expression loc sql
          buf.Append (fst evalResult)
        else
          writeEmbeddedVarComment expression buf
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) buf visitNode
      | ForBlock(forComment, nodeList) -> 
        writeForBlock (forComment, nodeList) buf visitNode
    let buf = visitStatement (StringBuilder(200)) statement
    buf.ToString()

  let prepareCore (config:IDbConfig) sql exprCtxt (parser:Func<string, Statement>) =
    let statement = parser.Invoke sql
    generate config sql exprCtxt statement

  let prepare (config:IDbConfig) sql exprCtxt parser =
    let exprCtxt = concatExprCtxt config.Dialect.RootExprCtxt exprCtxt
    prepareCore config sql exprCtxt parser

  let preparePaginate (config:IDbConfig) sql exprCtxt offset limit (parser:Func<string, Statement>) =
    let statement = parser.Invoke sql
    let exprCtxt = concatExprCtxt config.Dialect.RootExprCtxt exprCtxt
    let sql = resolveOrderByEmbeddedVariables config statement sql exprCtxt
    let statement = parser.Invoke sql
    let sql, exprCtxt = config.Dialect.RewriteForPagination (statement, sql, exprCtxt, offset, limit)
    let exprCtxt = concatExprCtxt config.Dialect.RootExprCtxt exprCtxt
    prepareCore config sql exprCtxt parser

  let preparePaginateAndCount (config:IDbConfig) sql exprCtxt offset limit (parser:Func<string, Statement>) =
    let statement = parser.Invoke sql
    let exprCtxt = concatExprCtxt config.Dialect.RootExprCtxt exprCtxt
    let sql = resolveOrderByEmbeddedVariables config statement sql exprCtxt
    let statement = parser.Invoke sql
    let newSql, newExprCtxt = config.Dialect.RewriteForCalcPagination (statement, sql, exprCtxt, offset, limit)
    let newExprCtxt = concatExprCtxt config.Dialect.RootExprCtxt newExprCtxt
    let paginatePs = prepareCore config newSql newExprCtxt parser
    let newSql, newExprCtxt = config.Dialect.RewriteForCount (statement, sql, exprCtxt)
    let newExprCtxt = concatExprCtxt config.Dialect.RootExprCtxt newExprCtxt
    let countPs = prepareCore config newSql newExprCtxt parser
    paginatePs, countPs

  let prepareFind (config:IDbConfig) (idList:obj list) (entityMeta:EntityMeta) =
    let buf = SqlBuilder(config.Dialect)
    buf.Append("select ")
    entityMeta.PropMetaList 
    |> List.iter (fun propMeta ->
      buf.Append(propMeta.SqlColumnName)
      buf.Append(", ") )
    buf.CutBack(2)
    buf.Append(" from ")
    buf.Append(entityMeta.SqlTableName)
    buf.Append(" where ")
    (idList, entityMeta.IdPropMetaList)
    ||> List.iter2 (fun id propMeta -> 
      buf.Append(propMeta.SqlColumnName)
      buf.Append(" = ")
      buf.Bind(id, propMeta.Type)
      buf.Append(" and "))
    buf.CutBack(5)
    buf.Build()

  let inline isTargetPropMeta (entity:obj) (opt: ^a) (propMeta:PropMeta) customExclusionRule =
    let exclud = (^a : (member Exclude: seq<string>) opt)
    let includ = (^a : (member Include: seq<string>) opt)
    let excludeNull = (^a : (member ExcludeNull: bool) opt)
    let propName = propMeta.PropName
    if customExclusionRule(propMeta.PropCase) then
      false
    elif (match propMeta.PropCase with Version _ -> true | _ -> false) then
      true
    elif excludeNull && propMeta.GetValue(entity) = null then
      false
    elif not <| Seq.isEmpty includ then
      Seq.exists ((=) propName) includ &&  not <| Seq.exists ((=) propName) exclud
    else
      not <| Seq.exists ((=) propName) exclud

  let prepareInsert (config:IDbConfig) (entity:obj) (entityMeta:EntityMeta) (opt:InsertOpt) =
    let propMetaSeq =
      entityMeta.PropMetaList
      |> Seq.filter (fun propMeta -> 
        propMeta.IsInsertable )
      |> Seq.filter (fun propMeta ->
        isTargetPropMeta entity opt propMeta (function Id Identity | Version VersionKind.Computed -> true | _ -> false) )
    if Seq.isEmpty propMetaSeq then
      raise <| NoInsertablePropertyException()
    let buf = SqlBuilder(config.Dialect)
    buf.Append("insert into ")
    buf.Append(entityMeta.SqlTableName)
    buf.Append(" ( ")
    propMetaSeq 
    |> Seq.iter (fun propMeta ->
      buf.Append(propMeta.SqlColumnName)
      buf.Append(", ") )
    buf.CutBack(2)
    buf.Append(" ) values ( ")
    propMetaSeq 
    |> Seq.iter (fun propMeta ->
      buf.Bind(propMeta.GetValue entity, propMeta.Type)
      buf.Append(", ") )
    buf.CutBack(2)
    buf.Append(" )")
    buf.Build()

  let prepareUpdate (config:IDbConfig) (entity:obj) (entityMeta:EntityMeta) (opt:UpdateOpt) =
    let incremetedVersion =
      entityMeta.VersionPropMeta
      |> Option.bind (fun propMeta -> 
        match propMeta.PropCase with
        | Version VersionKind.Incremented -> Some propMeta 
        | _ -> None)
    let propMetaSeq =
      entityMeta.PropMetaList
      |> Seq.filter (fun propMeta -> 
        propMeta.IsUpdatable )
      |> Seq.filter (fun propMeta -> 
        isTargetPropMeta entity opt propMeta (function Id _ | Version _ -> true | _ -> false) )
    if Seq.isEmpty propMetaSeq && (opt.IgnoreVersion || incremetedVersion.IsNone) then
      raise <| NoUpdatablePropertyException()
    let buf = SqlBuilder(config.Dialect)
    buf.Append("update ")
    buf.Append(entityMeta.SqlTableName)
    buf.Append(" set ")
    propMetaSeq
    |> Seq.iter (fun propMeta ->
      buf.Append(propMeta.SqlColumnName)
      buf.Append(" = ")
      buf.Bind(propMeta.GetValue(entity), propMeta.Type)
      buf.Append(", ") )
    buf.CutBack(2)
    if not opt.IgnoreVersion then
      incremetedVersion
      |> Option.iter (fun propMeta ->
        buf.Append ", "
        buf.Append (propMeta.SqlColumnName)
        buf.Append " = "
        buf.Append (propMeta.SqlColumnName)
        buf.Append " + 1" )
    buf.Append(" where ")
    entityMeta.IdPropMetaList 
    |> Seq.iter (fun propMeta -> 
      buf.Append(propMeta.SqlColumnName)
      buf.Append(" = ")
      buf.Bind(propMeta.GetValue(entity), propMeta.Type)
      buf.Append(" and ") )
    buf.CutBack(5)
    if not opt.IgnoreVersion then
      entityMeta.VersionPropMeta
      |> Option.iter (fun propMeta -> 
        buf.Append " and "
        buf.Append(propMeta.SqlColumnName)
        buf.Append(" = ")
        buf.Bind(propMeta.GetValue(entity), propMeta.Type) )
    buf.Build ()

  let prepareDelete (config:IDbConfig) (entity:obj) (entityMeta:EntityMeta) (opt:DeleteOpt) =
    let buf = SqlBuilder(config.Dialect)
    buf.Append("delete from ")
    buf.Append(entityMeta.SqlTableName)
    buf.Append(" where ")
    entityMeta.IdPropMetaList
    |> Seq.iter (fun propMeta -> 
      buf.Append(propMeta.SqlColumnName)
      buf.Append(" = ")
      buf.Bind(propMeta.GetValue(entity), propMeta.Type)
      buf.Append(" and "))
    buf.CutBack(5)
    if not opt.IgnoreVersion then
      entityMeta.VersionPropMeta
      |> Option.iter (fun propMeta -> 
        buf.Append " and "
        buf.Append(propMeta.SqlColumnName)
        buf.Append(" = ")
        buf.Bind(propMeta.GetValue(entity), propMeta.Type) )
    buf.Build ()

  let prepareCall (config:IDbConfig) procedure (procedureMeta:ProcedureMeta) =
    let getDirection = function
      | Input -> Direction.Input
      | InputOutput -> Direction.InputOutput
      | Output -> Direction.Output
      | ReturnValue -> Direction.ReturnValue
      | Result _ -> Direction.Result
      | Unit -> failwith "unreachable."
    let parameters = 
      procedureMeta.ProcedureParamMetaList
      |> Seq.filter (fun paramMeta -> 
          match paramMeta.ParamMetaCase with
          | Unit -> false
          | Result _ -> config.Dialect.IsResultParamRecognizedAsOutputParam
          | _ -> true )
      |> Seq.map (fun paramMeta -> 
        let value, typ, dbType = config.Dialect.ConvertFromClrToDb(paramMeta.GetValue procedure, paramMeta.Type, paramMeta.UdtTypeName)
        let value =
          match paramMeta.ParamMetaCase with
          | Input | InputOutput -> value
          | _ -> box DBNull.Value
        let name = config.Dialect.CreateParameterName paramMeta.ParamName
        { Name = name
          Value = value
          Type = typ
          DbType = dbType
          Direction = getDirection paramMeta.ParamMetaCase
          Size = paramMeta.Size
          Precision = paramMeta.Precision
          Scale = paramMeta.Scale
          UdtTypeName = paramMeta.UdtTypeName } )
      |> Seq.toList
    let procedureName = procedureMeta.SqlProcedureName
    { Text = procedureName
      FormattedText = config.Dialect.BuildProcedureCallSql(procedureName, parameters)
      Parameters = parameters }

[<AbstractClass>]
type DialectBase() as this = 

  [<DefaultValue>]
  val mutable lazyRootExprCtxt : System.Lazy<IDictionary<string, obj * Type>>

  let emptyDisposer = { new IDisposable with member this.Dispose() = () }

  do
    let isNullOrEmpty (obj:obj) = 
      match obj with
      | :? string as x -> 
        String.IsNullOrEmpty(x)
      | :? option<string> as x-> 
        match x with None -> true | Some x -> String.IsNullOrEmpty(x)
      | :? CharString as x -> 
        String.IsNullOrEmpty(x.Value)
      | :? option<CharString> as x-> 
        match x with None -> true | Some x -> String.IsNullOrEmpty(x.Value)
      | _ -> 
        if obj = null then true else String.IsNullOrEmpty(string obj)

    let isNullOrWhiteSpace (obj:obj) = 
      match obj with
      | :? string as x -> 
        String.IsNullOrWhiteSpace(x)
      | :? option<string> as x-> 
        match x with None -> true | Some x -> String.IsNullOrWhiteSpace(x)
      | :? CharString as x -> 
        String.IsNullOrWhiteSpace(x.Value)
      | :? option<CharString> as x-> 
        match x with None -> true | Some x -> String.IsNullOrWhiteSpace(x.Value)
      | _ -> 
        if obj = null then true else String.IsNullOrWhiteSpace(string obj)

    let date (obj:obj) = 
      match obj with
      | :? DateTime as x -> 
        Nullable x.Date
      | :? option<DateTime> as x-> 
        match x with 
        | Some x -> Nullable x.Date 
        | None -> Nullable()
      | :? Nullable<DateTime> as x-> 
        x
      | _ -> 
        if obj <> null then 
          let d = Convert.ToDateTime(obj)
          Nullable d.Date
        else
          Nullable()

    let nextDate (obj:obj) =
      let nullable = date obj
      if nullable.HasValue then
        let value = nullable.Value
        Nullable(value.Date.Add(TimeSpan(1, 0, 0, 0)))
      else
        nullable

    let prevDate (obj:obj) =
      let nullable = date obj
      if nullable.HasValue then
        let value = nullable.Value
        Nullable(value.Date.Subtract(TimeSpan(1, 0, 0, 0)))
      else
        nullable

    let escape (obj:obj) = 
      match obj with
      | :? string as x -> 
        this.EscapeMetaChars x
      | :? option<string> as x-> 
        match x with None -> null | Some x -> this.EscapeMetaChars x
      | :? CharString as x -> 
        this.EscapeMetaChars x.Value
      | :? option<CharString> as x-> 
        match x with None -> null | Some x -> this.EscapeMetaChars x.Value
      | _ -> 
        if obj = null then null else this.EscapeMetaChars(string obj)

    let startsWith (obj:obj) = 
      let s = escape obj
      if s = null then null else s + "%"

    let contains (obj:obj) = 
      let s = escape obj
      if s = null then null else "%" + s + "%"

    let endsWith (obj:obj) = 
      let s = escape obj
      if s = null then null else "%" + s

    let charString (obj:obj) =
      match obj with
      | :? string as x -> 
        CharString x
      | :? option<string> as x-> 
        match x with None -> null | Some x -> CharString x
      | :? CharString as x -> 
        x
      | :? option<CharString> as x-> 
        match x with None -> null | Some x -> x
      | _ ->
        if obj = null then null else CharString(obj.ToString())

    let charStringList (obj:obj) =
      match obj with
      | :? IEnumerable as e ->
        e |> Seq.cast<obj> |> Seq.map charString
      | _ -> 
        Seq.empty<CharString>

    let createRootExprCtxt () =
      let isNullOrEmptyType = isNullOrEmpty.GetType()
      let isNullOrWhiteSpaceType = isNullOrWhiteSpace.GetType()
      let dateType = date.GetType()
      let nextDateType = nextDate.GetType()
      let prevDateType = prevDate.GetType()
      let escapeType = escape.GetType()
      let startsWithType = startsWith.GetType()
      let containsType = contains.GetType()
      let endsWithType = endsWith.GetType()
      let charStringType = charString.GetType()
      let charStringListType = charStringList.GetType()
      dict [
        "isNullOrEmpty", (box isNullOrEmpty, isNullOrEmptyType)
        "@IsNullOrEmpty", (box isNullOrEmpty, isNullOrEmptyType)
        "isNullOrWhiteSpace", (box isNullOrWhiteSpace, isNullOrWhiteSpaceType)
        "@IsNullOrWhiteSpace", (box isNullOrWhiteSpace, isNullOrWhiteSpaceType)
        "date", (box date, dateType)
        "@Date", (box date, dateType)
        "nextDate", (box nextDate, nextDateType)
        "@NextDate", (box nextDate, nextDateType)
        "prevDate", (box prevDate, prevDateType)
        "@PrevDate", (box prevDate, prevDateType)
        "escape", (box escape, escapeType)
        "@Escape", (box escape, escapeType)
        "startsWith", (box startsWith, startsWithType)
        "@StartsWith", (box startsWith, startsWithType)
        "contains", (box contains, containsType)
        "@Contains", (box contains, containsType)
        "endsWith", (box endsWith, endsWithType)
        "@EndsWith", (box endsWith, endsWithType) 
        "charString", (box charString, charStringType)
        "@CharString", (box charString, charStringType)
        "charStringList", (box charStringList, charStringListType)
        "@CharStringList", (box charStringList, charStringListType) ]

    this.lazyRootExprCtxt <- lazy (createRootExprCtxt ())

  member this.ConcatExprCtxt(x:IDictionary<string, obj * Type>, y:IDictionary<string, obj * Type>) = 
    Sql.concatExprCtxt x y

  abstract CanGetIdentityAtOnce : bool
  default this.CanGetIdentityAtOnce = false

  abstract CanGetIdentityAndVersionAtOnce : bool
  default this.CanGetIdentityAndVersionAtOnce = false

  abstract CanGetVersionAtOnce : bool
  default this.CanGetVersionAtOnce = false

  abstract IsResultParamRecognizedAsOutputParam : bool
  default this.IsResultParamRecognizedAsOutputParam = false

  abstract IsHasRowsPropertySupported : bool
  default this.IsHasRowsPropertySupported = true

  abstract RootExprCtxt :  IDictionary<string, obj * Type>
  default this.RootExprCtxt = this.lazyRootExprCtxt.Value

  abstract CountFunction : string
  default this.CountFunction = "count"

  abstract EscapeMetaChars : string -> string

  abstract PrepareIdentitySelect : string * string -> PreparedStatement
  default this.PrepareIdentitySelect(tableName, idColumnName) = raise <| NotSupportedException("GetIdentitySelectSql")

  abstract PrepareIdentityAndVersionSelect : string * string * string -> PreparedStatement
  default this.PrepareIdentityAndVersionSelect(tableName, idColumnName, versionColumnName) = raise <| NotSupportedException("GetIdentityAndVersionSelectSql")

  abstract PrepareVersionSelect : string * string * list<string * obj * Type> -> PreparedStatement
  default this.PrepareVersionSelect(tableName, versionColumnName, idMetaList) = raise <| NotSupportedException("GetVersionSelectSql")

  abstract PrepareSequenceSelect : string -> PreparedStatement
  default this.PrepareSequenceSelect(sequenceName) = raise <| NotSupportedException("PrepareSequenceSelect")

  abstract ConvertFromDbToUnderlyingClr : obj * Type -> obj
  default this.ConvertFromDbToUnderlyingClr (dbValue:obj, destType:Type) = 
    if dbValue.GetType() = destType then
      dbValue
    elif destType = typeof<CharString> then
      upcast CharString(Convert.ToString(dbValue))
    else
      Convert.ChangeType(dbValue, destType)

  abstract ConvertFromDbToClr : obj * Type * string * PropertyInfo-> obj
  default this.ConvertFromDbToClr (dbValue:obj, destType:Type, udtTypeName:string, destProp:PropertyInfo) = 
    let toEnumObject (enumType:Type) underlyingValue =
      let underlyingType = enumType.GetEnumUnderlyingType()
      let underlyingValue = this.ConvertFromDbToUnderlyingClr(underlyingValue, underlyingType)
      Enum.ToObject(enumType, underlyingValue)
    if dbValue = null || Convert.IsDBNull(dbValue) then
      null
    elif dbValue.GetType() = destType then
      dbValue
    elif Reflection.isOptionType destType then
      let _, _, elementType = Reflection.getOptionInfo destType
      let value = 
        if elementType.IsEnum then 
          toEnumObject elementType dbValue
        else 
          this.ConvertFromDbToUnderlyingClr(dbValue, elementType)
      Reflection.makeOption (value, destType)
    elif Reflection.isNullableType destType then
      let elementType = Nullable.GetUnderlyingType(destType)
      let value =
        if elementType.IsEnum then 
          toEnumObject elementType dbValue
        else 
          this.ConvertFromDbToUnderlyingClr(dbValue, elementType)
      Reflection.makeNullable (value, destType)
    elif destType.IsEnum then
      toEnumObject destType dbValue
    else
      this.ConvertFromDbToUnderlyingClr(dbValue, destType)
 
  // http://msdn.microsoft.com/ja-jp/library/yy6y35y8.aspx
  // http://msdn.microsoft.com/ja-jp/library/cc716729.aspx
  abstract MapClrTypeToDbType : Type -> DbType
  default this.MapClrTypeToDbType (clrType:Type) =
    match clrType with
    | t when t = typeof<String> -> DbType.String
    | t when t = typeof<Int32> || t = typeof<Int32 Nullable> -> DbType.Int32
    | t when t = typeof<Int64> || t = typeof<Int64 Nullable> -> DbType.Int64
    | t when t = typeof<Decimal> || t = typeof<Decimal Nullable> -> DbType.Decimal
    | t when t = typeof<DateTime> || t = typeof<DateTime Nullable> -> DbType.DateTime
    | t when t = typeof<TimeSpan> || t = typeof<TimeSpan Nullable> -> DbType.Time
    | t when t = typeof<Boolean> || t = typeof<Boolean Nullable> -> DbType.Boolean
    | t when t = typeof<CharString> -> DbType.StringFixedLength
    | t when t = typeof<Byte> || t = typeof<Byte Nullable> -> DbType.Byte
    | t when t = typeof<Byte[]> -> DbType.Binary
    | t when t = typeof<DateTimeOffset> || t = typeof<DateTimeOffset Nullable> -> DbType.DateTimeOffset
    | t when t = typeof<Double> || t = typeof<Double Nullable> -> DbType.Double
    | t when t = typeof<Single> || t = typeof<Single Nullable> -> DbType.Single
    | t when t = typeof<Guid> || t = typeof<Guid Nullable> -> DbType.Guid
    | t when t = typeof<Int16> || t = typeof<Int16 Nullable> -> DbType.Int16
    | t when t = typeof<UInt16> || t = typeof<UInt16 Nullable> -> DbType.UInt16
    | t when t = typeof<UInt32> || t = typeof<UInt32 Nullable> -> DbType.UInt32
    | t when t = typeof<UInt64> || t = typeof<UInt64 Nullable> -> DbType.UInt64
    | t when t = typeof<Object> -> DbType.Object
    | _ -> DbType.String
  
  abstract ConvertFromClrToDb : obj * Type * string -> obj * Type * DbType
  default this.ConvertFromClrToDb (clrValue:obj, srcType:Type, udtTypeName:string) = 
    let value, typ = 
      if clrValue = null || Convert.IsDBNull(clrValue) then 
        let typ =
          if Reflection.isOptionType srcType then
            snd <| Reflection.getOptionElement (null, srcType)
          elif Reflection.isNullableType srcType then
            Nullable.GetUnderlyingType srcType
          else
            srcType
        if typ.IsEnum then
          Convert.DBNull, typ.GetEnumUnderlyingType()
        else 
          Convert.DBNull, typ
      else
        let value, typ =
          let typ = clrValue.GetType()
          if Reflection.isOptionType typ then
            Reflection.getOptionElement (clrValue, typ)
          elif Reflection.isNullableType typ then
            Reflection.getNullableElement (clrValue, typ)
          else
            clrValue, typ
        let value =
          if typ = typeof<CharString> then
            match value with
            | :? CharString as cs -> cs.Value :> obj
            | _ -> value
          else
            value
        if typ.IsEnum then
          let text = Enum.Format(typ, value, "D")
          match typ.GetEnumUnderlyingType() with
          | t when t = typeof<Int32> -> upcast int32 text, t
          | t when t = typeof<Int64> -> upcast int64 text, t
          | t when t = typeof<Byte> -> upcast byte text, t
          | t when t = typeof<SByte> -> upcast sbyte text, t
          | t when t = typeof<Int16> -> upcast int16 text, t
          | t when t = typeof<UInt16> -> upcast uint16 text, t
          | t when t = typeof<UInt32> -> upcast uint32 text, t
          | t when t = typeof<UInt64> -> upcast uint64 text, t
          | _ -> failwith "unreachable."
        else 
          value, typ
    value, typ, this.MapClrTypeToDbType typ
  
  abstract FormatAsSqlLiteral : obj * Type * DbType -> string
  default this.FormatAsSqlLiteral (dbValue:obj, clrType:Type, dbType:DbType) =
    if Convert.IsDBNull dbValue then 
      "null"
    else 
      let quote () = "'" + string dbValue + "'"
      match dbType with
      | d when d = DbType.String || d = DbType.StringFixedLength->
        "N'" + string dbValue + "'"
      | d when d = DbType.Time ->
        match dbValue with
        | :? TimeSpan as t -> 
          "'" + t.ToString("c") + "'"
        | _ -> 
          quote ()
      | d when d = DbType.Date ->
        match dbValue with
        | :? DateTime as d -> 
          "'" + d.ToString("yyyy-MM-dd") + "'"
        | _ -> 
          quote ()
      | d when d = DbType.DateTime ->
        match dbValue with
        | :? DateTime as d -> 
          "'" + d.ToString("yyyy-MM-dd HH:mm:ss.fff") + "'"
        | _ -> 
          quote ()
      | d when d = DbType.DateTime2 ->
        match dbValue with
        | :? DateTime as d -> 
          "'" + d.ToString("yyyy-MM-dd HH:mm:ss.fffffff") + "'"
        | _ -> 
          quote ()
      | d when d = DbType.DateTimeOffset ->
        match dbValue with
        | :? DateTimeOffset as d -> 
          "'" + d.ToString("yyyy-MM-dd HH:mm:ss K") + "'"
        | _ -> 
          quote ()
      | d when d = DbType.Binary ->
        "/** binary value is not shown */null"
      | d when d = DbType.Boolean ->
        match dbValue with
        | :? bool as b -> 
          if b then "'1'" else "'0'"
        | _ -> 
          quote ()
      | d when d = DbType.Guid ->
          quote ()
      | _ -> 
        string dbValue
  
  abstract CreateParameterName : int -> string
  default this.CreateParameterName (index:int) =
    "@p" + string index
  
  abstract CreateParameterName : string -> string
  default this.CreateParameterName (baseName:string) =
    if baseName.StartsWith "@" then
      baseName
    else
      "@" + baseName
 
  abstract IsUniqueConstraintViolation : exn -> bool
  
  abstract RewriteForPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  default this.RewriteForPagination (statement, sql, exprCtxt, offset, limit) = 
    this.RewriteForPaginationWithRowNumber(statement, sql, exprCtxt, offset, limit)
  
  abstract RewriteForCalcPagination : SqlAst.Statement * string * IDictionary<string, obj * Type> * int64 * int64 -> string * IDictionary<string, obj * Type>
  default this.RewriteForCalcPagination (statement, sql, exprCtxt, offset, limit) = 
    this.RewriteForPaginationWithRowNumber(statement, sql, exprCtxt, offset, limit)
  
  member this.RewriteForPaginationWithRowNumber (statement, sql, exprCtxt, offset, limit) = 
    let offset = if offset < 0L then 0L else offset
    let level = ref 0
    let orderByBuf = StringBuilder(100)
    let forUpdateBuf = StringBuilder(100)
    let rec visitStatement (buf:StringBuilder) = 
      function
      | Statement nodeList -> 
        List.fold visitNode buf nodeList
      | Set(set, lhs, rhs, _) ->
        writeSet (set, lhs, rhs) buf visitStatement
    and visitNode (buf:StringBuilder) =
      function
      | Word(fragment) ->
        let fragment =
          if buf = orderByBuf then
            let pos = fragment.LastIndexOf '.'
            if pos > 0 then
              "temp_" + fragment.Substring(pos, fragment.Length - pos)
            else
              fragment
          else
            fragment
        buf.Append fragment
      | Other(fragment)
      | Literal(fragment) 
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        buf.Append fragment
      | OrderBy(keyword, nodeList, _) ->
        let orderByBuf = if !level = 0 then orderByBuf else buf
        orderByBuf.Append keyword |> ignore
        List.fold visitNode orderByBuf nodeList |> ignore
        buf 
      | ForUpdate(keyword, nodeList, _) ->
        let forUpdateBuf = if !level = 0 then forUpdateBuf else buf
        forUpdateBuf.Append keyword |> ignore
        List.fold visitNode forUpdateBuf nodeList |> ignore
        buf 
      | Select(keyword, nodeList, _)
      | From(keyword, nodeList, _) 
      | Where(keyword, nodeList, _)
      | GroupBy(keyword, nodeList, _)
      | Having(keyword, nodeList, _)
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        buf.Append keyword |> ignore
        List.fold visitNode buf nodeList
      | Parens(statement) ->
        writeParens statement level buf visitStatement
      | BindVarComment(expression, node, _)
      | BindVarsComment(expression, node, _) ->
        writeBindVarComment (expression, node) buf visitNode
      | EmbeddedVarComment(expression, _)->
        writeEmbeddedVarComment expression buf
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) buf visitNode
      | ForBlock(forComment, nodeList) -> 
        writeForBlock (forComment, nodeList) buf visitNode
    let subqueryBuf = visitStatement (StringBuilder(200)) statement
    if orderByBuf.Length = 0 then
      let message = Sql.appendSimpleDetail (SR.SOMA2016 ()) sql
      raise <| SqlException (message)
    let buf = StringBuilder(400)
    buf.Append "select * from ( select temp_.*, row_number() over( " |> ignore
    buf.Append orderByBuf |> ignore
    buf.Append " ) as soma_rownumber_ from ( " |> ignore
    buf.Append subqueryBuf |> ignore
    buf.Append ") temp_ ) temp2_ where " |> ignore
    buf.Append "soma_rownumber_ > " |> ignore
    buf.Append "/* soma_offset */" |> ignore
    buf.Append offset |> ignore
    if limit >= 0L then
      buf.Append " and " |> ignore
      buf.Append "soma_rownumber_ <= " |> ignore
      buf.Append "/* soma_offset + soma_limit */" |> ignore
      buf.Append (offset + limit) |> ignore
    if forUpdateBuf.Length > 0 then
      buf.Append " " |> ignore
      buf.Append forUpdateBuf |> ignore
    let exprCtxt = Dictionary<string, obj * Type>(exprCtxt) :> IDictionary<string, obj * Type>
    exprCtxt.["soma_offset"] <- (box offset, typeof<int>)
    exprCtxt.["soma_limit"] <- (box limit, typeof<int>)
    buf.ToString(), exprCtxt
  
  abstract RewriteForCount : SqlAst.Statement * string * IDictionary<string, obj * Type> -> string * IDictionary<string, obj * Type>
  default this.RewriteForCount (statement, sql, exprCtxt) = 
    let level = ref 0
    let rec visitStatement (buf:StringBuilder) = 
      function
      | Statement nodeList -> 
        List.fold visitNode buf nodeList
      | Set(set, lhs, rhs, _) ->
        writeSet (set, lhs, rhs) buf visitStatement
    and visitNode buf =
      function
      | Word(fragment)
      | Other(fragment)
      | Literal(fragment) 
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        buf.Append fragment
      | OrderBy(keyword, nodeList, _) ->
        if !level <> 0 then
          buf.Append keyword |> ignore
          List.fold visitNode buf nodeList
        else
          buf
      | Select(keyword, nodeList, _)
      | From(keyword, nodeList, _) 
      | Where(keyword, nodeList, _)
      | GroupBy(keyword, nodeList, _)
      | Having(keyword, nodeList, _)
      | ForUpdate(keyword, nodeList, _)
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        buf.Append keyword |> ignore
        List.fold visitNode buf nodeList
      | Parens(statement) ->
        writeParens statement level buf visitStatement
      | BindVarComment(expression, node, _)
      | BindVarsComment(expression, node, _) ->
        writeBindVarComment (expression, node) buf visitNode
      | EmbeddedVarComment(expression, _)->
        writeEmbeddedVarComment expression buf
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) buf visitNode
      | ForBlock(forComment, nodeList) -> 
        writeForBlock (forComment, nodeList) buf visitNode
    let buf = visitStatement (StringBuilder(200)) statement
    buf.Insert(0, "select " + this.CountFunction + "(*) from ( ") |> ignore
    buf.Append " ) t_" |> ignore
    buf.ToString(), exprCtxt
  
  abstract BuildProcedureCallSql : string * PreparedParameter seq -> string
 
  abstract EncloseIdentifier : string -> string
 
  abstract SetupDbParameter : PreparedParameter * DbParameter -> unit
  default this.SetupDbParameter(param, dbParam) =
    let dbParam = dbParam :> IDbDataParameter
    dbParam.ParameterName <- param.Name
    dbParam.DbType <- param.DbType
    dbParam.Direction <-
      match param.Direction with
      | Direction.Input -> ParameterDirection.Input
      | Direction.InputOutput -> ParameterDirection.InputOutput
      | Direction.Output -> ParameterDirection.Output
      | Direction.ReturnValue -> ParameterDirection.ReturnValue
      | Direction.Result -> ParameterDirection.Output
      | _ -> failwith "unreachable."
    Option.iter (fun size -> dbParam.Size <- size) param.Size
    Option.iter (fun precision -> dbParam.Precision <- precision) param.Precision
    Option.iter (fun scale -> dbParam.Scale <- scale) param.Scale
    dbParam.Value <- param.Value

  abstract GetValue : DbDataReader * int * PropertyInfo -> obj
  default this.GetValue(reader, index, destProp) =
    reader.GetValue(index)

  abstract MakeParametersDisposer : DbCommand -> IDisposable
  default this.MakeParametersDisposer(command) = 
    emptyDisposer
    
  interface IDialect with
    member this.CanGetIdentityAtOnce = this.CanGetIdentityAtOnce
    member this.CanGetIdentityAndVersionAtOnce = this.CanGetIdentityAndVersionAtOnce
    member this.CanGetVersionAtOnce = this.CanGetVersionAtOnce
    member this.IsResultParamRecognizedAsOutputParam = this.IsResultParamRecognizedAsOutputParam
    member this.IsHasRowsPropertySupported = this.IsHasRowsPropertySupported
    member this.RootExprCtxt = this.RootExprCtxt
    member this.EscapeMetaChars(text) = this.EscapeMetaChars(text)
    member this.PrepareIdentitySelect(tableName, idColumnName) = this.PrepareIdentitySelect(tableName, idColumnName)
    member this.PrepareIdentityAndVersionSelect(tableName, idColumnName, versionColumnName) = this.PrepareIdentityAndVersionSelect(tableName, idColumnName, versionColumnName)
    member this.PrepareVersionSelect(tableName, versionColumnName, idMetaList) = this.PrepareVersionSelect(tableName, versionColumnName, idMetaList)
    member this.PrepareSequenceSelect(sequenceName) = this.PrepareSequenceSelect(sequenceName)
    member this.ConvertFromDbToClr(value, typ, string, destProp) = this.ConvertFromDbToClr(value, typ, string, destProp)
    member this.ConvertFromClrToDb(value, typ, string) = this.ConvertFromClrToDb(value, typ, string)
    member this.FormatAsSqlLiteral(value, clrType, dbType) = this.FormatAsSqlLiteral(value, clrType, dbType)
    member this.CreateParameterName(index:int) = this.CreateParameterName(index)
    member this.CreateParameterName(baseName:string) = this.CreateParameterName(baseName)
    member this.IsUniqueConstraintViolation(exn) = this.IsUniqueConstraintViolation (exn)
    member this.RewriteForPagination(statement, sql, exprCtxt, offset, limit) = this.RewriteForPagination(statement, sql, exprCtxt, offset, limit)
    member this.RewriteForCalcPagination(statement, sql, exprCtxt, offset, limit) = this.RewriteForCalcPagination(statement, sql, exprCtxt, offset, limit)
    member this.RewriteForCount(statement, sql, exprCtxt) =  this.RewriteForCount(statement, sql, exprCtxt)
    member this.BuildProcedureCallSql(procedureName, parameters) = this.BuildProcedureCallSql(procedureName, parameters)
    member this.EncloseIdentifier(identifier) = this.EncloseIdentifier(identifier)
    member this.SetupDbParameter(param, dbParam) = this.SetupDbParameter(param, dbParam)
    member this.GetValue(reader, index, destProp) = this.GetValue(reader, index, destProp)
    member this.MakeParametersDisposer(command) = this.MakeParametersDisposer(command)

type MsSqlDialect() = 
  inherit DialectBase()

  let escapeRegex = Regex(@"[$_%\[]")

  override this.CanGetIdentityAtOnce = true

  override this.CanGetIdentityAndVersionAtOnce = true

  override this.CanGetVersionAtOnce = true

  override this.CountFunction = "count_big"

  override this.EscapeMetaChars(text) =
    escapeRegex.Replace(text, "$$$&")

  override this.PrepareIdentitySelect(tableName, idColumnName) = 
    let buf = SqlBuilder this
    buf.Append "select scope_identity() where @@rowcount = 1"
    buf.Build()

  override this.PrepareIdentityAndVersionSelect(tableName, idColumnName, versionColumnName) = 
    let buf = SqlBuilder this
    buf.Append "select scope_identity(), " 
    buf.Append versionColumnName 
    buf.Append " from "
    buf.Append tableName
    buf.Append " where "
    buf.Append idColumnName
    buf.Append " = scope_identity() and @@rowcount = 1"
    buf.Build()

  override this.PrepareVersionSelect(tableName, versionColumnName, idMetaList) = 
    let buf = SqlBuilder (dialect = this, parameterNameSuffix = "x")
    buf.Append "select "
    buf.Append versionColumnName
    buf.Append " from "
    buf.Append tableName
    buf.Append " where "
    idMetaList
    |> List.iter (fun (idColumnName, idValue, idType) -> 
      buf.Append idColumnName
      buf.Append " = "
      buf.Bind(idValue, idType)
      buf.Append " and ")
    buf.Append "@@rowcount = 1"
    buf.Build()
 
  override this.IsUniqueConstraintViolation(exn:exn) = 
    match exn with
    | :? System.Data.SqlClient.SqlException as ex -> 
      match ex.Number with
      | 2601 | 2627 -> true
      | _ -> false
    | _ -> 
      false
 
  override this.RewriteForPagination (statement, sql, exprCtxt, offset, limit) =
    if (offset <= 0L && limit > 0L) || limit = 0L then
      this.RewriteForPaginationWithTop (statement, sql, exprCtxt, limit)
    else
      base.RewriteForPagination (statement, sql, exprCtxt, offset, limit)
 
  override this.RewriteForCalcPagination (statement, sql, exprCtxt, offset, limit) =
    if (offset <= 0L && limit > 0L) || limit = 0L then
      this.RewriteForPaginationWithTop (statement, sql, exprCtxt, limit)
    else
      base.RewriteForCalcPagination (statement, sql, exprCtxt, offset, limit)

  member this.RewriteForPaginationWithTop (statement, sql, exprCtxt, limit) =
    let limit = if limit < 0L then 0L else limit
    let level = ref 0
    let rec visitStatement (buf:StringBuilder) = 
      function
      | Statement nodeList -> 
        List.fold visitNode buf nodeList
      | Set(set, lhs, rhs, _) ->
        writeSet (set, lhs, rhs) buf visitStatement
    and visitNode buf =
      function
      | Word(fragment)
      | Other(fragment)
      | Literal(fragment) 
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        buf.Append(fragment)
      | Select(keyword, nodeList, loc) ->
        buf.Append keyword |> ignore
        if !level = 0 then
          buf.Append " top " |> ignore
          buf.Append "(/* soma_limit */" |> ignore
          buf.Append limit |> ignore
          buf.Append ")" |> ignore
        List.fold visitNode buf nodeList
      | From(keyword, nodeList, _) 
      | Where(keyword, nodeList, _)
      | GroupBy(keyword, nodeList, _)
      | Having(keyword, nodeList, _)
      | OrderBy(keyword, nodeList, _)
      | ForUpdate(keyword, nodeList, _) 
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        let buf = buf.Append keyword
        List.fold visitNode buf nodeList
      | Parens(statement) ->
        writeParens statement level buf visitStatement
      | BindVarComment(expression, node, _)
      | BindVarsComment(expression, node, _) ->
        writeBindVarComment (expression, node) buf visitNode
      | EmbeddedVarComment(expression, _)->
        writeEmbeddedVarComment expression buf
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) buf visitNode
      | ForBlock(forComment, nodeList) -> 
        writeForBlock (forComment, nodeList) buf visitNode
    let buf = visitStatement (StringBuilder(200) )statement
    let exprCtxt = Dictionary<string, obj * Type>(exprCtxt) :> IDictionary<string, obj * Type>
    exprCtxt.["soma_limit"] <- (box limit, typeof<int>)
    buf.ToString(), exprCtxt
 
  override this.BuildProcedureCallSql(procedureName, parameters) = 
    let getDataType (p:PreparedParameter) =
      match p.DbType with
      | d when d = DbType.Int32 -> "int"
      | d when d = DbType.Int64 -> "bigint"
      | d when d = DbType.Boolean -> "bit"
      | d when d = DbType.Date -> "date"
      | d when d = DbType.DateTime -> "datetime"
      | d when d = DbType.DateTime2 -> "datetime2"
      | d when d = DbType.DateTimeOffset -> "datetimeoffset"
      | d when d = DbType.Decimal -> "decimal"
      | d when d = DbType.Binary -> "varbinary"
      | d when d = DbType.Double -> "float"
      | d when d = DbType.String -> "nvarchar"
      | d when d = DbType.StringFixedLength -> "nchar"
      | d when d = DbType.Int16 -> "smallint"
      | d when d = DbType.Time -> "time"
      | d when d = DbType.Byte -> "tinyint"
      | d when d = DbType.Guid -> "uniqueidentifier"
      | _ -> "sql_variant"
    let getInitValue (p:PreparedParameter) =
      match p.Direction with
      | Direction.Input
      | Direction.InputOutput -> 
        this.FormatAsSqlLiteral(p.Value, p.Type, p.DbType)
      | _ -> 
        "null"
    let buf = StringBuilder 200
    parameters
    |> Seq.iter (fun p -> 
      buf 
      +> "declare" +> " " +> p.Name +> " " +> (getDataType p) +> "\n" 
      +> "set" +> " " +> p.Name +> " = " +> (getInitValue p) +! "\n" )
    buf 
    +> "exec " 
    +> (parameters
        |> Seq.tryFind (fun p -> p.Direction = Direction.ReturnValue)
        |> function Some p -> p.Name + " = " | _ -> "")
    +> procedureName +! " "
    parameters
    |> Seq.filter (fun p ->
      match p.Direction with
      | Direction.Input | Direction.InputOutput | Direction.Output -> true 
      | _ -> false)
    |> Seq.peek
    |> Seq.iter (fun (p, hasNext) ->
      match p.Direction with
      | Direction.Input ->
        buf +! p.Name
      | Direction.InputOutput
      | Direction.Output -> 
        buf +> p.Name +! " output"
      | _ -> ()
      if hasNext then buf +! ", ")
    buf.ToString()
 
  override this.EncloseIdentifier(identifier) = "[" + identifier + "]"

  // http://msdn.microsoft.com/ja-jp/library/yy6y35y8.aspx
  override this.SetupDbParameter(param, dbParam) =
    base.SetupDbParameter(param, dbParam)
    match dbParam with
    | :? System.Data.SqlClient.SqlParameter as dbParam -> 
      match param.DbType with
      | d when d = DbType.Binary -> dbParam.SqlDbType <- SqlDbType.VarBinary
      | d when d = DbType.String -> dbParam.SqlDbType <- SqlDbType.NVarChar
      | d when d = DbType.StringFixedLength -> dbParam.SqlDbType <- SqlDbType.NChar
      | d when d = DbType.Date -> dbParam.SqlDbType <- SqlDbType.Date
      | d when d = DbType.DateTimeOffset -> dbParam.SqlDbType <- SqlDbType.DateTimeOffset
      | d when d = DbType.Time -> dbParam.SqlDbType <- SqlDbType.Time
      | _ -> 
        ()
    | _ ->
      ()

type MsSqlCeDialect() = 
  inherit DialectBase()

  let escapeRegex = Regex(@"[$_%]")

  override this.CountFunction = "count"

  override this.EscapeMetaChars(text) =
    escapeRegex.Replace(text, "$$$&")

  override this.PrepareIdentitySelect(tableName, idColumnName) = 
    let buf = SqlBuilder this
    buf.Append "select @@identity"
    buf.Build()

  override this.PrepareIdentityAndVersionSelect(tableName, idColumnName, versionColumnName) = 
    let buf = SqlBuilder this
    buf.Append "select @@identity, " 
    buf.Append versionColumnName 
    buf.Append " from "
    buf.Append tableName
    buf.Append " where "
    buf.Append idColumnName
    buf.Append " = @@identity"
    buf.Build()

  override this.PrepareVersionSelect(tableName, versionColumnName, idMetaList) = 
    let buf = SqlBuilder (dialect = this, parameterNameSuffix = "x")
    buf.Append "select "
    buf.Append versionColumnName
    buf.Append " from "
    buf.Append tableName
    buf.Append " where "
    idMetaList
    |> List.iter (fun (idColumnName, idValue, idType) -> 
      buf.Append idColumnName
      buf.Append " = "
      buf.Bind(idValue, idType)
      buf.Append " and ")
    buf.CutBack 5
    buf.Build()

  override this.IsUniqueConstraintViolation(exn:exn) = 
    let typ = exn.GetType()
    let prop = typ.GetProperty("NativeError", typeof<int>)
    prop <> null &&
    match prop.GetValue(exn, null) :?> int with
    | 25016 -> true
    | _ -> false
 
  override this.RewriteForPagination (statement, sql, exprCtxt, offset, limit) =
    if (offset <= 0L && limit > 0L) || limit = 0L then
      this.RewriteForPaginationWithTop (statement, sql, exprCtxt, limit)
    else
      this.RewriteForPaginationWithOffsetFetchNext (statement, sql, exprCtxt, offset, limit)
 
  override this.RewriteForCalcPagination (statement, sql, exprCtxt, offset, limit) =
    if (offset <= 0L && limit > 0L) || limit = 0L then
      this.RewriteForPaginationWithTop (statement, sql, exprCtxt, limit)
    else
      this.RewriteForPaginationWithOffsetFetchNext (statement, sql, exprCtxt, offset, limit)

  member this.RewriteForPaginationWithTop (statement, sql, exprCtxt, limit) =
    let limit = if limit < 0L then 0L else limit
    let level = ref 0
    let rec visitStatement (buf:StringBuilder) = 
      function
      | Statement nodeList -> 
        List.fold visitNode buf nodeList
      | Set(set, lhs, rhs, _) ->
        writeSet (set, lhs, rhs) buf visitStatement
    and visitNode buf =
      function
      | Word(fragment)
      | Other(fragment)
      | Literal(fragment) 
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        buf.Append(fragment)
      | Select(keyword, nodeList, loc) ->
        buf.Append keyword |> ignore
        if !level = 0 then
          buf.Append " top " |> ignore
          buf.Append "(/* soma_limit */" |> ignore
          buf.Append limit |> ignore
          buf.Append ")" |> ignore
        List.fold visitNode buf nodeList
      | From(keyword, nodeList, _) 
      | Where(keyword, nodeList, _)
      | GroupBy(keyword, nodeList, _)
      | Having(keyword, nodeList, _)
      | OrderBy(keyword, nodeList, _)
      | ForUpdate(keyword, nodeList, _) 
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        let buf = buf.Append keyword
        List.fold visitNode buf nodeList
      | Parens(statement) ->
        writeParens statement level buf visitStatement
      | BindVarComment(expression, node, _)
      | BindVarsComment(expression, node, _) ->
        writeBindVarComment (expression, node) buf visitNode
      | EmbeddedVarComment(expression, _)->
        writeEmbeddedVarComment expression buf
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) buf visitNode
      | ForBlock(forComment, nodeList) -> 
        writeForBlock (forComment, nodeList) buf visitNode
    let buf = visitStatement (StringBuilder(200) )statement
    let exprCtxt = Dictionary<string, obj * Type>(exprCtxt) :> IDictionary<string, obj * Type>
    exprCtxt.["soma_limit"] <- (box limit, typeof<int>)
    buf.ToString(), exprCtxt

  member this.RewriteForPaginationWithOffsetFetchNext (statement, sql, exprCtxt, offset, limit) =
    let offset = if offset < 0L then 0L else offset
    let level = ref 0
    let isOrderByExistent = ref false
    let rec visitStatement (buf:StringBuilder) = 
      function
      | Statement nodeList -> 
        List.fold visitNode buf nodeList
      | Set(set, lhs, rhs, _) ->
        writeSet (set, lhs, rhs) buf visitStatement
    and visitNode buf =
      function
      | Word(fragment)
      | Other(fragment)
      | Literal(fragment) 
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        buf.Append(fragment)
      | Select(keyword, nodeList, _)
      | From(keyword, nodeList, _) 
      | Where(keyword, nodeList, _)
      | GroupBy(keyword, nodeList, _)
      | Having(keyword, nodeList, _)
      | ForUpdate(keyword, nodeList, _) 
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        let buf = buf.Append keyword
        List.fold visitNode buf nodeList
      | OrderBy(keyword, nodeList, _) ->
        if (not !isOrderByExistent) && !level = 0 then
          isOrderByExistent := true
        let buf = buf.Append keyword
        List.fold visitNode buf nodeList
      | Parens(statement) ->
        writeParens statement level buf visitStatement
      | BindVarComment(expression, node, _)
      | BindVarsComment(expression, node, _) ->
        writeBindVarComment (expression, node) buf visitNode
      | EmbeddedVarComment(expression, _)->
        writeEmbeddedVarComment expression buf
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) buf visitNode
      | ForBlock(forComment, nodeList) -> 
        writeForBlock (forComment, nodeList) buf visitNode
    let buf = visitStatement (StringBuilder(200) )statement
    if not !isOrderByExistent then
      let message = Sql.appendSimpleDetail (SR.SOMA2017 ()) sql
      raise <| SqlException (message)
    buf.Append " offset /* soma_offset */" |> ignore
    buf.Append offset |> ignore
    buf.Append " rows" |> ignore
    if limit > 0L then
      buf.Append " fetch next /* soma_limit */" |> ignore
      buf.Append limit |> ignore
      buf.Append " rows only" |> ignore
    let exprCtxt = Dictionary<string, obj * Type>(exprCtxt) :> IDictionary<string, obj * Type>
    exprCtxt.["soma_offset"] <- (box offset, typeof<int>)
    exprCtxt.["soma_limit"] <- (box limit, typeof<int>)
    buf.ToString(), exprCtxt

  override this.BuildProcedureCallSql(procedureName, parameters) = 
    raise <| NotSupportedException("BuildProcedureCallSql")
 
  override this.EncloseIdentifier(identifier) = "[" + identifier + "]"

  override this.IsHasRowsPropertySupported = false

type MySqlDialect() = 
  inherit DialectBase()
 
  let escapeRegex = Regex(@"[$_%]")
 
  override this.EscapeMetaChars(text) =
    escapeRegex.Replace(text, "$$$&")
  
  override this.PrepareIdentitySelect(tableName, idColumnName) = 
    let buf = SqlBuilder this
    buf.Append "select last_insert_id()"
    buf.Build()
  
  override this.IsUniqueConstraintViolation(exn:exn) = 
    match exn with
    | :? DbException as ex ->
      let prop = ex.GetType().GetProperty("Number", typeof<int>)
      prop <> null &&
      match prop.GetValue(ex, null) :?> int with
      | 1022 
      | 1062 -> true
      | _ -> false
    | _ -> false
  
  override this.RewriteForPagination (statement, sql, exprCtxt, offset, limit) =
    this.RewriteForPaginationWithOffsetLimit (statement, exprCtxt, offset, limit, false)
  
  override this.RewriteForCalcPagination (statement, sql, exprCtxt, offset, limit) =
    this.RewriteForPaginationWithOffsetLimit (statement, exprCtxt, offset, limit, true)
  
  member this.RewriteForPaginationWithOffsetLimit (statement, exprCtxt, offset, limit, calc:bool) = 
    let offset = if offset < 0L then 0L else offset
    let limit = if limit < 0L then UInt64.MaxValue else uint64 limit
    let level = ref 0
    let forUpdateBuf = StringBuilder(100)
    let rec visitStatement (buf:StringBuilder) = 
      function
      | Statement nodeList -> 
        List.fold visitNode buf nodeList
      | Set(set, lhs, rhs, _) ->
        writeSet (set, lhs, rhs) buf visitStatement
    and visitNode (buf:StringBuilder) =
      function
      | Word(fragment)
      | Other(fragment)
      | Literal(fragment) 
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        buf.Append fragment
      | Select(keyword, nodeList, _) ->
        buf.Append keyword |> ignore
        if calc && !level = 0 then 
          buf.Append " sql_calc_found_rows" |> ignore
        List.fold visitNode buf nodeList
      | ForUpdate(keyword, nodeList, _) ->
        let forUpdateBuf = if !level = 0 then forUpdateBuf else buf
        forUpdateBuf.Append keyword |> ignore
        List.fold visitNode forUpdateBuf nodeList |> ignore
        buf 
      | From(keyword, nodeList, _) 
      | Where(keyword, nodeList, _)
      | GroupBy(keyword, nodeList, _)
      | Having(keyword, nodeList, _)
      | OrderBy(keyword, nodeList, _)
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        buf.Append keyword |> ignore
        List.fold visitNode buf nodeList
      | Parens(statement) ->
        writeParens statement level buf visitStatement
      | BindVarComment(expression, node, _)
      | BindVarsComment(expression, node, _) ->
        writeBindVarComment (expression, node) buf visitNode
      | EmbeddedVarComment(expression, _)->
        writeEmbeddedVarComment expression buf
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) buf visitNode
      | ForBlock(forComment, nodeList) -> 
        writeForBlock (forComment, nodeList) buf visitNode
    let buf = visitStatement (StringBuilder(200)) statement
    buf.Append " limit " |> ignore
    buf.Append "/* soma_offset */" |> ignore
    buf.Append offset |> ignore
    buf.Append ", " |> ignore
    buf.Append "/* soma_limit */" |> ignore
    buf.Append limit |> ignore
    if forUpdateBuf.Length > 0 then
      buf.Append " " |> ignore
      buf.Append forUpdateBuf |> ignore
    let exprCtxt = Dictionary<string, obj * Type>(exprCtxt) :> IDictionary<string, obj * Type>
    exprCtxt.["soma_offset"] <- (box offset, typeof<int>)
    exprCtxt.["soma_limit"] <- (box limit, typeof<int>)
    buf.ToString(), exprCtxt 

  override this.RewriteForCount (statement, sql, exprCtxt) = 
    "select found_rows()", Dictionary<string, obj * Type>(exprCtxt) :> IDictionary<string, obj * Type>
 
  override this.BuildProcedureCallSql(procedureName, parameters) =
    let buf = StringBuilder 200
    buf +> procedureName +! " "
    parameters
    |> Seq.peek
    |> Seq.iter (fun (p, hasNext) ->
      buf 
      +> p.Name 
      +> " = " 
      +! (if p.Value = null then "null" else string p.Value)
      if hasNext then buf +! ", " )
    buf.ToString()
 
  override this.EncloseIdentifier(identifier) = "`" + identifier + "`"
 
  override this.SetupDbParameter(param, dbParam) =
    base.SetupDbParameter(param, dbParam)

type OracleDialect() = 
  inherit DialectBase()
 
  let escapeRegex = Regex(@"[$_%＿％]")
 
  override this.EscapeMetaChars(text) =
    escapeRegex.Replace(text, "$$$&")
 
  override this.PrepareSequenceSelect(sequenceName) = 
    let buf = SqlBuilder this
    buf.Append "select "
    buf.Append sequenceName
    buf.Append ".nextval from dual"
    buf.Build()
 
  override this.IsResultParamRecognizedAsOutputParam = true
 
  override this.IsUniqueConstraintViolation(exn:exn) = 
    match exn with
    | :? DbException as ex ->
      let prop = ex.GetType().GetProperty("Number", typeof<int>)
      prop <> null &&
      match prop.GetValue(ex, null) :?> int with
      | 1 -> true
      | _ -> false
    | _ -> false
 
  override this.CreateParameterName (index:int) =
    ":p" + string index
 
  override this.CreateParameterName (baseName:string) =
    baseName
 
  override this.BuildProcedureCallSql(procedureName, parameters) =
    let buf = StringBuilder 200
    buf +> procedureName +! " "
    parameters
    |> Seq.peek
    |> Seq.iter (fun (p, hasNext) ->
      buf 
      +> p.Name 
      +> " = " 
      +! (if p.Value = null then "null" else string p.Value)
      if hasNext then buf +! ", " )
    buf.ToString()
 
  override this.EncloseIdentifier(identifier) = "\"" + identifier + "\""

  override this.ConvertFromDbToUnderlyingClr (dbValue, destType) = 
    let (|ConvertibleToBoolean|_|) (dbValue:obj, destType) =
      if destType = typeof<bool> then
        match dbValue with
        | :? byte 
        | :? int16 
        | :? int32
        | :? int64 
        | :? decimal
        | :? uint16 
        | :? uint32 
        | :? uint64 
         ->
          try
            Some (box (Convert.ToInt32(dbValue) = 1))
          with
          | exn ->
            Some (box false)
        | _ ->
          Some (box false)
      else
        None
    let (|ConvertibleToTimeSpan|_|) (dbValue, destType) =
      if destType = typeof<TimeSpan> then
        let typ = dbValue.GetType()
        let assmebly = typ.Assembly
        if typ = assmebly.GetType("Oracle.DataAccess.Types.OracleIntervalDS") then
          let prop = typ.GetProperty("Value")
          Some (prop.GetValue(dbValue, null))
        else
          None
      else 
        None
    match dbValue, destType with
    | ConvertibleToBoolean(obj) 
    | ConvertibleToTimeSpan(obj) -> 
      obj
    | _ -> 
      base.ConvertFromDbToUnderlyingClr(dbValue, destType)
  
  override this.ConvertFromClrToDb (clrValue, srcType, udtTypeName) = 
    let dbValue, typ, dbType = base.ConvertFromClrToDb (clrValue, srcType, udtTypeName)
    match dbType with
    | d when d = DbType.Boolean -> 
      let dbValue =
        if Convert.IsDBNull dbValue then
          dbValue
        else 
          box (if true.Equals dbValue then 1 else 0)
      dbValue, typeof<int>, DbType.Int32
    | _ ->
      dbValue, typ, dbType
 
  override this.FormatAsSqlLiteral (dbValue:obj, clrType:Type, dbType:DbType) = 
    if Convert.IsDBNull dbValue then 
      "null"
    else
      let literal =
        match dbType with
        | d when d = DbType.String || d = DbType.StringFixedLength ->
            Some ("'" + string dbValue + "'")
        | d when d = DbType.Time ->
          match dbValue with
          | :? TimeSpan as t -> 
            Some ( "time '" + t.ToString("c") + "'")
          | _ -> 
            None
        | d when d = DbType.Date ->
          match dbValue with
          | :? DateTime as d -> 
            Some ("date '" + d.ToString("yyyy-MM-dd") + "'")
          | _ -> 
            None
        | d when d = DbType.DateTime ->
          match dbValue with
          | :? DateTime as d -> 
            Some ("timestamp '" + d.ToString("yyyy-MM-dd HH:mm:ss.fffffff") + "'")
          | _ -> 
            None
        | d when d = DbType.Object->
          match dbValue with
          | :? TimeSpan as d -> 
            Some (String.Format("interval '{0} {1}:{2}:{3}.{4}' day to second", d.ToString("dd"), d.ToString("hh"), d.ToString("mm"), d.ToString("ss"), d.ToString("fffffff")))
          | _ -> 
            None
        | _ ->
          None
      match literal with
      | Some value -> value
      | _ -> base.FormatAsSqlLiteral (dbValue, clrType, dbType)

  override this.SetupDbParameter(param, dbParam) =
    base.SetupDbParameter(param, dbParam)
    let dbParamType = dbParam.GetType()
    let assembly = dbParamType.Assembly
    let setOracleDbType typeName =
      let prop = dbParamType.GetProperty("OracleDbType")
      let enumType = assembly.GetType("Oracle.DataAccess.Client.OracleDbType")
      if prop <> null && enumType <> null then
        let enumValue = Enum.Parse(enumType, typeName)
        prop.SetValue(dbParam, enumValue, null)
    if param.Direction = Direction.Result then
      setOracleDbType "RefCursor"
    if param.DbType = DbType.Time then
      setOracleDbType "IntervalDS"
    if param.UdtTypeName <> null then
      let dbTypeName = if param.Type.IsArray then "Array" else "Object"
      setOracleDbType dbTypeName
      let prop = dbParamType.GetProperty("UdtTypeName")
      if prop <> null then
        prop.SetValue(dbParam, param.UdtTypeName, null)
  
  override this.MakeParametersDisposer(command) = 
    { new IDisposable with 
      member this.Dispose() =
        let dispose : obj -> unit = function
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()
        let count = command.Parameters.Count
        for i = 0 to count - 1 do
          let p = command.Parameters.[i]
          dispose p.Value
          dispose p }

type SQLiteDialect() = 
  inherit DialectBase()
 
  let escapeRegex = Regex(@"[$_%]")
 
  override this.EscapeMetaChars(text) =
    escapeRegex.Replace(text, "$$$&")
  
  override this.PrepareIdentitySelect(tableName, idColumnName) = 
    let buf = SqlBuilder this
    buf.Append "select "
    buf.Append idColumnName
    buf.Append " from "
    buf.Append tableName
    buf.Append " where rowid = last_insert_rowid()"
    buf.Build()
  
  override this.IsUniqueConstraintViolation(exn:exn) = 
    let exnType = exn.GetType()
    let assembly = exnType.Assembly
    let sqliteExnType = assembly.GetType("System.Data.SQLite.SQLiteException")
    let enumType = assembly.GetType("System.Data.SQLite.SQLiteErrorCode")
    if sqliteExnType <> null && enumType <> null && sqliteExnType.IsAssignableFrom exnType then
      let prop = sqliteExnType.GetProperty("ErrorCode", enumType)
      if prop <> null then
        let errorCode = prop.GetValue(exn, null)
        errorCode = Enum.Parse(enumType, "Constraint")
      else
        false
    else
      false
  
  override this.RewriteForPagination (statement, sql, exprCtxt, offset, limit) =
    this.RewriteForPaginationWithOffsetLimit (statement, exprCtxt, offset, limit)
  
  override this.RewriteForCalcPagination (statement, sql, exprCtxt, offset, limit) =
    this.RewriteForPaginationWithOffsetLimit (statement, exprCtxt, offset, limit)
  
  member this.RewriteForPaginationWithOffsetLimit (statement, exprCtxt, offset, limit) = 
    let offset = if offset < 0L then 0L else offset
    let limit = if limit < 0L then Int64.MaxValue else limit
    let level = ref 0
    let rec visitStatement (buf:StringBuilder) = 
      function
      | Statement nodeList -> 
        List.fold visitNode buf nodeList
      | Set(set, lhs, rhs, _) ->
        writeSet (set, lhs, rhs) buf visitStatement
    and visitNode (buf:StringBuilder) =
      function
      | Word(fragment)
      | Other(fragment)
      | Literal(fragment) 
      | Whitespaces(fragment)
      | Newline(fragment) 
      | BlockComment(fragment)
      | LineComment(fragment) ->
        buf.Append fragment
      | Select(keyword, nodeList, _)
      | From(keyword, nodeList, _) 
      | Where(keyword, nodeList, _)
      | GroupBy(keyword, nodeList, _)
      | Having(keyword, nodeList, _)
      | OrderBy(keyword, nodeList, _)
      | ForUpdate(keyword, nodeList, _)
      | And(keyword, nodeList)
      | Or(keyword, nodeList) ->
        buf.Append keyword |> ignore
        List.fold visitNode buf nodeList
      | Parens(statement) ->
        writeParens statement level buf visitStatement
      | BindVarComment(expression, node, _)
      | BindVarsComment(expression, node, _) ->
        writeBindVarComment (expression, node) buf visitNode
      | EmbeddedVarComment(expression, _)->
        writeEmbeddedVarComment expression buf
      | IfBlock(ifComment, elifCommentList, elseComment, nodeList) ->
        writeIfBlock (ifComment, elifCommentList, elseComment, nodeList) buf visitNode
      | ForBlock(forComment, nodeList) -> 
        writeForBlock (forComment, nodeList) buf visitNode
    let buf = visitStatement (StringBuilder(200)) statement
    buf.Append " limit " |> ignore
    buf.Append "/* soma_offset */" |> ignore
    buf.Append offset |> ignore
    buf.Append ", " |> ignore
    buf.Append "/* soma_limit */" |> ignore
    buf.Append limit |> ignore
    let exprCtxt = Dictionary<string, obj * Type>(exprCtxt) :> IDictionary<string, obj * Type>
    exprCtxt.["soma_offset"] <- (box offset, typeof<int>)
    exprCtxt.["soma_limit"] <- (box limit, typeof<int>)
    buf.ToString(), exprCtxt
 
  override this.BuildProcedureCallSql(procedureName, parameters) =
    raise <| NotSupportedException("BuildProcedureCallSql")
 
  override this.EncloseIdentifier(identifier) = "\"" + identifier + "\""

  override this.FormatAsSqlLiteral (dbValue:obj, clrType:Type, dbType:DbType) =
    if Convert.IsDBNull dbValue then 
      base.FormatAsSqlLiteral(dbValue, clrType, dbType)
    else
      match dbType with
      | d when d = DbType.String ->
        "'" + string dbValue + "'"
      | _ ->
        base.FormatAsSqlLiteral(dbValue, clrType, dbType)

  override this.ConvertFromDbToUnderlyingClr (dbValue:obj, destType:Type) = 
    match dbValue with
    | :? (byte[]) as bytes when destType = typeof<Guid> -> 
      upcast (Guid(bytes))
    | _ -> 
      base.ConvertFromDbToUnderlyingClr(dbValue, destType)
