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

namespace Soma.Core.UT

module SqlTest = 

  open System
  open System.Collections.Generic
  open System.Data
  open System.Globalization
  open Microsoft.FSharp.Text.Lexing
  open NUnit.Framework
  open Soma.Core
  open Soma.Core.SqlAst

  let private (|SingleNode|_|) statement = 
    match statement with Statement ([h]) -> Some h | _ -> None

  let config = 
    { new MsSqlConfig() with
      member this.ConnectionString = "" }

  [<Test>]
  let ``parse Set : union`` () =
    let test s = 
      match Sql.parse s with Set _ -> () | x -> fail x
    test "select 1 union select 2" 
    test "select 1 union all select 2" 
    test "select 1 UNION select 2"
    test "select 1 UNION ALL select 2"

  [<Test>]
  let ``parse Set : union : subquery`` () =
    let test s = 
      match Sql.parse s with SingleNode (Select _) -> () | x -> fail x
    test "select 1 from (select 2 union select 3)" 

  [<Test>]
  let ``parse Set : minus`` () =
    let test s =
      match Sql.parse s with Set _ -> () | x -> fail x
    test "select 1 minus select 2"
    test "select 1 minus all select 2"
    test "select 1 MINUS select 2"
    test "select 1 MINUS ALL select 2"

  [<Test>]
  let ``parse Set : except`` () =
    let test sql = 
      match Sql.parse sql with Set _ -> () | x -> fail x
    test "select 1 except select 2" 
    test "select 1 except all select 2"
    test "select 1 EXCEPT select 2"
    test "select 1 EXCEPT ALL select 2"

  [<Test>]
  let ``parse Set : intersect`` () =
    let test sql = 
      match Sql.parse sql with Set _ -> () | x -> fail x
    test "select 1 intersect select 2"
    test "select 1 intersect all select 2"
    test "select 1 INTERSECT select 2" 
    test "select 1 INTERSECT ALL select 2"

  [<Test>]
  let ``parse Select`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Select _) -> () | x -> fail x
    test "select" 
    test "SELECT" 
    test "sElEcT"

  [<Test>]
  let ``parse From`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (From _) -> () | x -> fail x
    test "from"
    test "FROM"
    test "fRoM"

  [<Test>]
  let ``parse Where`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Where _) -> () | x -> fail x
    test "where"
    test "WHERE"
    test "wHeRe"

  [<Test>]
  let ``parse Having`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Having _) -> () | x -> fail x
    test "having"
    test "HAVING"
    test "hAvInG"

  [<Test>]
  let ``parse GroupBy`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (GroupBy _) -> () | x -> fail x
    test "group by" 
    test "GROUP BY" 
    test "gRoUp   By"

  [<Test>]
  let ``parse OrderBy`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (OrderBy _) -> () | x -> fail x
    test "order by" 
    test "ORDER BY" 
    test "oRdEr   By"

  [<Test>]
  let ``parse ForUpdate`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (ForUpdate _) -> () | x -> fail x
    test "for update" 
    test "FOR UPDATE" 
    test "fOr  UpDaTe"

  [<Test>]
  let ``parse And`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (And _) -> () | x -> fail x
    test "and" 
    test "AND" 
    test "aNd" 

  [<Test>]
  let ``parse Or`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Or _) -> () | x -> fail x
    test "or" 
    test "OR" 
    test "oR" 

  [<Test>]
  let ``parse Parens`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Parens _) -> () | x -> fail x
    test "()" 
    test "( )"
    test "(select)"

  [<Test>]
  let ``parse IfBlock : if`` () =
    let test sql expr = 
      match Sql.parse sql with 
      | SingleNode (IfBlock (IfComment (x, _, _), _, _, _)) when x = expr -> () 
      | x -> fail x
    test "/*%if aaa *//*%end */" "aaa"
    test "/*% if aaa *//*%end */" "aaa"
    test "/*% \n if aaa *//*%end */" "aaa"

  [<Test>]
  let ``parse IfBlock : single elif`` () =
    let test sql expr = 
      match Sql.parse sql with 
      | SingleNode (IfBlock (_, elifCommentList, _, _)) ->
        match elifCommentList with
        | [ElifComment (x, _, _)] when x = expr -> ()
        | x -> fail x
      | x -> fail x
    test "/*%if aaa *//*%elif bbb *//*%end */" "bbb"
    test "/*%if aaa *//*% elif bbb *//*%end */" "bbb"
    test "/*%if aaa *//*% \n elif bbb *//*%end */" "bbb"

  [<Test>]
  let ``parse IfBlock : multiple elif`` () =
    let test sql expr1 expr2 = 
      match Sql.parse sql with 
      | SingleNode (IfBlock (_, elifCommentList, _, _)) ->
        match elifCommentList with
        | [ElifComment (x, _, _); ElifComment (y, _, _)] when x = expr1 && y = expr2 -> () 
        | x -> fail x
      | x -> fail x
    test "/*%if aaa *//*%elif bbb *//*%elif ccc *//*%end */" "bbb" "ccc"

  [<Test>]
  let ``parse IfBlock : else`` () =
    let test sql = 
      match Sql.parse sql with 
      | SingleNode (IfBlock (_, _, Some (ElseComment _), _)) -> () 
      | x -> fail x
    test "/*%if aaa *//*%else*//*%end */"
    test "/*%if aaa *//*% else*//*%end */"
    test "/*%if aaa *//*% \n else*//*%end */"

  [<Test>]
  let ``parse IfBlock : end`` () =
    let test sql = 
      match Sql.parse sql with 
      | SingleNode (IfBlock (_, _, _, _)) -> () 
      | x -> fail x
    test "/*%if aaa *//*%end */"
    test "/*%if aaa *//*% end */"
    test "/*%if aaa *//*% \n end */"

  [<Test>]
  let ``parse IfComment : if comment is not closed`` () =
    try
      Sql.parse "/*%if aaa" |> ignore
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2010" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``parse ForBlock : for`` () =
    let test sql = 
      match Sql.parse sql with 
      | SingleNode (ForBlock (ForComment _, _)) -> () 
      | x -> fail x
    test "/*%for aaa *//*%end */"
    test "/*% for aaa *//*%end */"
    test "/*% \n for aaa *//*%end */"

  [<Test>]
  let ``parse ForBlock : end`` () =
    let test sql = 
      match Sql.parse sql with 
      | SingleNode (ForBlock (_, _)) -> () 
      | x -> fail x
    test "/*%for aaa *//*%end */"
    test "/*%for aaa *//*% end */"
    test "/*%for aaa *//*% \n end */"

  [<Test>]
  let ``parse unknown expression directive`` () =
    try
      Sql.parse "/*%hoge aaa *//*%end */" |> ignore
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2019" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``parse Word`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Word _) -> () | x -> fail x
    test "abc" 
    test "\"abc\.\"def\"" 
    test "[abc].[def]" 

  [<Test>]
  let ``parse Other`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Other _) -> () | x -> fail x
    test "+"
    test "-"
    test "*"

  [<Test>]
  let ``parse BlockComment`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (BlockComment _) -> () | x -> fail x
    test "/** aaa */"
    test "/*+ aaa */"
    test "/*: aaa */"
    test "/** /* */"

  [<Test>]
  let ``parse BlockComment : block comment is not closed`` () =
    try
      Sql.parse "/** aaa" |> ignore
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2011" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``parse LineComment`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (LineComment _) -> () | x -> fail x
    test "-- aaa"
    test "-- aaa\n"

  [<Test>]
  let ``parse Literal`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Literal _) -> () | x -> fail x
    test "'aaa'"
    test "'aa''a'"
    test "123"
    test "+123"
    test "-123"
    test ".123"
    test "123.456"
    test "123.456e"
    test "123.456E"
    test "123.456e10"
    test "date  '2011-01-23'"
    test "date'2011-01-23'"
    test "DATE  '2011-01-23'"
    test "DATE'2011-01-23'"
    test "time '12:34:56'"
    test "time'12:34:56'"
    test "TIME '12:34:56'"
    test "TIME'12:34:56'"
    test "timestamp '2011-01-23 12:34:56'"
    test "timestamp'2011-01-23 12:34:56'"
    test "TIMESTAMP '2011-01-23 12:34:56'"
    test "TIMESTAMP'2011-01-23 12:34:56'"
    test "true"
    test "TRUE"
    test "false"
    test "FALSE"
    test "null"
    test "NULL"

  [<Test>]
  let ``parse Literal : single quote is not closed`` () =
    try
      Sql.parse "'aaa" |> ignore
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2012" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``parse BindVarComment`` () =
    let test sql expr = 
      match Sql.parse sql with SingleNode (BindVarComment (x, _, _)) when x = expr -> () | x -> fail x
    test "/* aaa */'bbb'" "aaa"
    test "/*aaa*/'bbb'" "aaa"

  [<Test>]
  let ``parse BindVarsComment`` () =
    let test sql expr = 
      match Sql.parse sql with SingleNode (BindVarsComment (x, _, _)) when x = expr -> () | x -> fail x
    test "/* aaa */('bbb')" "aaa"
    test "/*aaa*/('bbb')" "aaa"

  [<Test>]
  let ``parse EmbeddedVarComment`` () =
    let test sql expr = 
      match Sql.parse sql with SingleNode (EmbeddedVarComment (x, _)) when x = expr -> () | x -> fail x
    test "/*# aaa */" "aaa"
    test "/*#aaa*/" "aaa"

  [<Test>]
  let ``parse Statement`` () =
    let test sql = 
      match Sql.parse sql with Statement _ -> () | x -> fail x
    test "select * from aaa where bbb = /* ccc */'ddd'"

  [<Test>]
  let ``parse Whitespaces`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Whitespaces _) -> () | x -> fail x
    test " "
    test "    "
    test "\t"
    test "\t\t"
    test "  \t"

  [<Test>]
  let ``parse Newline`` () =
    let test sql = 
      match Sql.parse sql with SingleNode (Newline _) -> () | x -> fail x
    test "\n"
    test "\r\n"

  let parser = Func<string, Statement>(Sql.parse)

  [<Test>]
  let ``prepare : simple select`` () =
    let ps = Sql.prepare config "select * from aaa where bbb = 1" Map.empty parser
    assert_equal "select * from aaa where bbb = 1" ps.Text
    
  [<Test>]
  let ``prepare : sub select`` () =
    let ps = 
      Sql.prepare 
        config 
        "select * from aaa where bbb = /* bbb */'hoge' and ccc = (select ddd from eee where fff = /* fff */'') and ggg = /* ggg */''"
        (dict ["bbb", (box "0", typeof<string>); "fff", (box "1", typeof<string>); "ggg", (box "2", typeof<string>)])
        parser
    assert_equal "select * from aaa where bbb = @p0 and ccc = (select ddd from eee where fff = @p1) and ggg = @p2" ps.Text

  [<Test>]
  let ``prepare : bind var`` () =
    let ps = Sql.prepare config "select * from aaa where bbb = /*bbb*/1" (dict ["bbb", (box 1, typeof<int>)]) parser
    assert_equal "select * from aaa where bbb = @p0" ps.Text

  [<Test>]
  let ``prepare : bind var : no test literal right after bind var`` () =
    try
      Sql.prepare config "select * from aaa where bbb = /*bbb*/ 0 and 1 = 2" (dict ["bbb", (box 1, typeof<int>)]) parser |> ignore
      fail ()
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2008" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``prepare : bind var : no test literal`` () =
    try
      Sql.prepare config "select * from aaa where bbb = /*bbb*/ and 1 = 2" (dict ["bbb", (box 1, typeof<int>)]) parser |> ignore
      fail ()
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2008" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``prepare : bind var : no test literal before EOF`` () =
    try
      Sql.prepare config "select * from aaa where bbb = /*bbb*/" (dict ["bbb", (box 1, typeof<int>)]) parser |> ignore
      fail ()
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2009" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``prepare : bind var : not found`` () =
    try
      Sql.prepare config "select * from aaa where bbb = /*bbb*/1" (dict ["aaa", (box 1, typeof<int>)]) parser |> ignore
      fail ()
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2007" ex.MessageId
    | ex ->
      fail ex

  [<Test>]
  let ``prepare : bind vars`` () =
    let list = [10; 20; 30]
    let ps = Sql.prepare config "select * from aaa where bbb in /*bbb*/(1,2,3)" (dict ["bbb", (box list, list.GetType())]) parser
    assert_equal "select * from aaa where bbb in (@p0, @p1, @p2)" ps.Text
    
  [<Test>]
  let ``prepare : embedded var`` () =
    let ps = Sql.prepare config "select * from aaa /*# 'order by bbb' */" Map.empty parser
    assert_equal "select * from aaa order by bbb" ps.Text

  [<Test>]
  let ``prepare : top : where`` () =
    let ps = 
      Sql.prepare 
        config 
        "select top (/*top*/0) * from aaa where /*% if true */bbb = /*bbb*/1 /*% end */" 
        (dict ["bbb", (box 1, typeof<int>); "top", (box 10, typeof<int>)]) parser
    assert_equal "select top (@p0) * from aaa where bbb = @p1" ps.Text

  [<Test>]
  let ``prepare IfComment where left`` () =
    let ps = Sql.prepare config "select * from aaa where /*% if true */bbb = /*bbb*/1 /*% end */" (dict ["bbb", (box 1, typeof<int>)]) parser
    assert_equal "select * from aaa where bbb = @p0" ps.Text

  [<Test>]
  let ``prepare IfComment : where removed`` () =
    let ps = Sql.prepare config "select * from aaa where /*% if false */bbb = /*bbb*/1 /*% end */" (dict ["bbb", (box 1, typeof<int>)]) parser
    assert_equal "select * from aaa" ps.Text

  [<Test>]
  let ``prepare IfComment : where left : and left`` () =
    let ps = 
      Sql.prepare 
        config 
        "select * from aaa where /*% if true */bbb = /*bbb*/1 /*% end *//*% if true */and ccc = /*ccc*/1 /*% end */ " 
        (dict ["bbb", (box 1, typeof<int>); "ccc", (box 2, typeof<int>)]) parser
    assert_equal "select * from aaa where bbb = @p0 and ccc = @p1" ps.Text

  [<Test>]
  let ``prepare IfComment : where removed : and removed`` () =
    let ps = 
      Sql.prepare 
        config 
        "select * from aaa where /*% if false */bbb = /*bbb*/1 /*% end *//*% if true */and ccc = /*ccc*/1 /*% end */ " 
        (dict ["bbb", (box 1, typeof<int>); "ccc", (box 2, typeof<int>)]) parser
    assert_equal "select * from aaa where  ccc = @p0" ps.Text

  [<Test>]
  let ``prepare IfComment : parens removed : and removed`` () =
    let ps = 
      Sql.prepare 
        config 
        "select * from aaa where (/*% if false */bbb = /*bbb*/1 /*% end */) and ccc = /*ccc*/1" 
        (dict ["bbb", (box 1, typeof<int>); "ccc", (box 2, typeof<int>)]) parser
    assert_equal "select * from aaa where   ccc = @p0" ps.Text

  [<Test>]
  let ``prepare IfComment : parens left : and left`` () =
    let ps = 
      Sql.prepare 
        config 
        "select * from aaa where (/*% if true */bbb = /*bbb*/1 /*% end */) /*% if true */and ccc = /*ccc*/1 /*% end */" 
        (dict ["bbb", (box 1, typeof<int>); "ccc", (box 2, typeof<int>)]) parser
    assert_equal "select * from aaa where (bbb = @p0 ) and ccc = @p1" ps.Text

  [<Test>]
  let ``prepare IfComment : empty parens left : and left`` () =
    let ps = 
      Sql.prepare 
        config 
        "select * from aaa where bbb = getdate() and ccc = /*ccc*/1" 
        (dict ["ccc", (box 2, typeof<int>)]) parser
    assert_equal "select * from aaa where bbb = getdate() and ccc = @p0" ps.Text

  [<Test>]
  let ``prepare IfComment : whitespace parens left : and left`` () =
    let ps = 
      Sql.prepare 
        config 
        "select * from aaa where bbb = getdate(   ) and ccc = /*ccc*/1" 
        (dict ["ccc", (box 2, typeof<int>)]) parser
    assert_equal "select * from aaa where bbb = getdate(   ) and ccc = @p0" ps.Text

  [<Test>]
  let ``prepare IfComment : end comment is missing`` () =
    try
      Sql.prepare config "select * from aaa where /*% if bbb <> null */bbb = /*bbb*/1 and 1 = 1" (dict ["bbb", (box 1, typeof<int>)]) parser |> ignore
      fail ()
    with 
    | :? SqlException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA2005" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``prepare ElseComment : if comment is missing`` () =
    try
      Sql.prepare config "select * from aaa where /*% else */bbb = /*bbb*/1 and 1 = 1" (dict ["bbb", (box 1, typeof<int>)]) parser |> ignore
      fail ()
    with 
    | :? SqlException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA2004" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``prepare ElifComment : if comment is missing`` () =
    try
      Sql.prepare config "select * from aaa where /*% elif false */bbb = /*bbb*/1 and 1 = 1" (dict ["bbb", (box 1, typeof<int>)]) parser |> ignore
      fail ()
    with 
    | :? SqlException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA2004" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``prepare IfBlock : isn't enclosed in a clause`` () =
    try
      Sql.prepare config "select * from aaa /*% if bbb <> null */ where bbb = /*bbb*/1 /*% end */and 1 = 1" (dict ["bbb", (box 1, typeof<int>)]) parser |> ignore
      fail ()
    with 
    | :? SqlException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA2015" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``prepare union`` () =
    let ps = Sql.prepare config "select * from aaa union select * from bbb" Map.empty parser
    assert_equal "select * from aaa union select * from bbb" ps.Text

  [<Test>]
  let ``prepare ForComment`` () =
    let seq = [1; 2; 3]
    let ps = 
      Sql.prepare config "
        select * from aaa 
        where 
        /*% for bbb in seq */bbb = /*bbb*/1 
          /*% if bbb_has_next */
            /*# 'and' */
          /*% end */
        /*% end */
        " (dict ["seq", (box seq, seq.GetType())]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 3 ps.Parameters.Length
    assert_equal 1 ps.Parameters.[0].Value
    assert_equal 2 ps.Parameters.[1].Value
    assert_equal 3 ps.Parameters.[2].Value

  [<Test>]
  let ``preparePaginate ForComment : embedded variable property access in the where clause`` () =
    let seq = ["1"; "22"; "333"]
    let ps = 
      Sql.preparePaginate config "
        select * from aaa 
        where 
        /*% for bbb in seq */bbb/*#bbb.Length*/ = /*bbb*/1 
          /*% if bbb_has_next */
            /*# 'and' */
          /*% end */
        /*% end */
        order by
          aaa.id
        " (dict ["seq", (box seq, seq.GetType())]) 1L 10L parser
    printfn "%s" ps.Text
    let sql = "select * from ( select temp_.*, row_number() over( order by
          temp_.id
         ) as soma_rownumber_ from ( 
        select * from aaa 
        where 
        bbb1 = @p0 
          
            and
          
        bbb2 = @p1 
          
            and
          
        bbb3 = @p2 
          
        
        ) temp_ ) temp2_ where soma_rownumber_ > @p3 and soma_rownumber_ <= @p4"
    assert_equal sql ps.Text
    assert_equal 5 ps.Parameters.Length
    assert_equal "1" ps.Parameters.[0].Value
    assert_equal "22" ps.Parameters.[1].Value
    assert_equal "333" ps.Parameters.[2].Value
    assert_equal 1L ps.Parameters.[3].Value
    assert_equal 11L ps.Parameters.[4].Value

  [<Test>]
  let ``prepare ForComment : illegal expression`` () =
    let seq = [1; 2; 3]
    try
      Sql.prepare config "/*% for seq *//*% end */" (dict ["seq", (box seq, seq.GetType())]) parser |> ignore
      fail ()
    with 
    | :? SqlException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA2014" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``prepare ForBlock : isn't enclosed in a clause`` () =
    let seq = [1; 2; 3]
    try
      Sql.prepare config "
        select * from aaa 
        /*% for bbb in seq */bbb = /*bbb*/1 
        where
          /*% if bbb_has_next */
            /*# 'and' */
          /*% end */
        /*% end */
        " (dict ["seq", (box seq, seq.GetType())]) parser |> ignore
      fail ()
    with 
    | :? SqlException as ex -> 
      printfn "%s" ex.Message
      assert_equal "SOMA2015" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``prepare : dialect root expr : escape`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* escape bbb */'a' escape '$'" (dict ["bbb", (box "x%x", typeof<string>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "x$%x" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : escape : null`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* escape bbb */'a' escape '$'" (dict ["bbb", (null, typeof<obj>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : escape : option`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* escape bbb */'a' escape '$'" (dict ["bbb", (box (Some "x%x"), typeof<option<string>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "x$%x" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : Escape`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* @Escape(bbb) */'a' escape '$'" (dict ["bbb", (box "x%x", typeof<string>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "x$%x" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : startsWith`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* startsWith bbb */'a' escape '$'" (dict ["bbb", (box "x%x", typeof<string>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "x$%x%" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : startsWith : null`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* startsWith bbb */'a' escape '$'" (dict ["bbb", (null, typeof<obj>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : startsWith : option`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* startsWith bbb */'a' escape '$'" (dict ["bbb", (box (Some "x%x"), typeof<option<string>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "x$%x%" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : StartsWith`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* @StartsWith bbb */'a' escape '$'" (dict ["bbb", (box "x%x", typeof<string>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "x$%x%" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : contains`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* contains bbb */'a' escape '$'" (dict ["bbb", (box "x%x", typeof<string>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "%x$%x%" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : contains : null`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* contains bbb */'a' escape '$'" (dict ["bbb", (null, typeof<obj>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : contains : option`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* contains bbb */'a' escape '$'" (dict ["bbb", (box(Some "x%x"), typeof<option<string>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "%x$%x%" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : Contains`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* @Contains bbb */'a' escape '$'" (dict ["bbb", (box "x%x", typeof<string>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "%x$%x%" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : endsWith`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* endsWith bbb */'a' escape '$'" (dict ["bbb", (box "x%x", typeof<string>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "%x$%x" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : endsWith : null`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* endsWith bbb */'a' escape '$'" (dict ["bbb", (null, typeof<obj>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : endsWith : option`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* endsWith bbb */'a' escape '$'" (dict ["bbb", (box(Some "x%x"), typeof<option<string>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "%x$%x" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : EndsWith`` () =
    let ps = Sql.prepare config "select * from aaa where bbb like /* @EndsWith bbb */'a' escape '$'" (dict ["bbb", (box "x%x", typeof<string>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal "%x$%x" ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : isNullOrEmpty`` () =
    let ps = Sql.prepare config "select * from aaa where /*% if not (isNullOrEmpty bbb) */ bbb = /* bbb */'a'/*% end*/" (dict ["bbb", (box "", typeof<string>)]) parser
    assert_equal "select * from aaa" ps.Text

  [<Test>]
  let ``prepare : dialect root expr : isNullOrEmpty : null`` () =
    let ps = Sql.prepare config "select * from aaa where /*% if not (isNullOrEmpty bbb) */ bbb = /* bbb */'a'/*% end*/" (dict ["bbb", (null, typeof<obj>)]) parser
    assert_equal "select * from aaa" ps.Text

  [<Test>]
  let ``prepare : dialect root expr : isNullOrEmpty : option`` () =
    let ps = Sql.prepare config "select * from aaa where /*% if not (isNullOrEmpty bbb) */ bbb = /* bbb */'a'/*% end*/" (dict ["bbb", (box (Some ""), typeof<option<string>>)]) parser
    assert_equal "select * from aaa" ps.Text

  [<Test>]
  let ``prepare : dialect root expr : IsNullOrEmpty`` () =
    let ps = Sql.prepare config "select * from aaa where /*% if ! @IsNullOrEmpty(bbb) */ bbb = /* bbb */'a'/*% end*/" (dict ["bbb", (box "", typeof<string>)]) parser
    assert_equal "select * from aaa" ps.Text

  [<Test>]
  let ``prepare : dialect root expr : date`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* date bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box bbb, typeof<DateTime>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 23)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : date : null`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* date bbb */'2000-01-01 00:00:00'" (dict ["bbb", (null, typeof<obj>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : date : option`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* date bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box (Some bbb), typeof<option<DateTime>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 23)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : date : Nullable`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* date bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box (Nullable bbb), typeof<Nullable<DateTime>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 23)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : Date`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* @Date bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box bbb, typeof<DateTime>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 23)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : nextDate`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* nextDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box bbb, typeof<DateTime>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 24)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : nextDate : null`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* nextDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (null, typeof<obj>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : nextDate : option`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* nextDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box (Some bbb), typeof<option<DateTime>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 24)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : nextDate : Nullable`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* nextDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box (Nullable bbb), typeof<Nullable<DateTime>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 24)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : NextDate`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* @NextDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box bbb, typeof<DateTime>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 24)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : prevDate`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* prevDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box bbb, typeof<DateTime>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 22)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : prevDate : null`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* prevDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (null, typeof<obj>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : prevDate : option`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* prevDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box (Some bbb), typeof<option<DateTime>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 22)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : prevDate Nullable`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* prevDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box (Nullable bbb), typeof<Nullable<DateTime>>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 22)) ps.Parameters.[0].Value

  [<Test>]
  let ``prepare : dialect root expr : PrevDate`` () =
    let bbb = DateTime(2011, 1, 23, 12, 13, 14)
    let ps = Sql.prepare config "select * from aaa where bbb > /* @PrevDate bbb */'2000-01-01 00:00:00'" (dict ["bbb", (box bbb, typeof<DateTime>)]) parser
    printfn "%s" ps.Text
    printfn "%A" ps.Parameters
    assert_equal 1 ps.Parameters.Length
    assert_equal (DateTime(2011, 1, 22)) ps.Parameters.[0].Value

  [<Test>]
  let resolveOrderByEmbeddedVariables () =
    let sql = "select * from xxx.aaa where xxx.aaa.bbb = /* bbb */'a' and xxx.aaa.ddd = /*#ddd*/ order by /*#orderby*/"
    let statement = config.SqlParser.Invoke sql
    let sql = Sql.resolveOrderByEmbeddedVariables config statement sql  (dict [("orderby", (box "xxx.aaa.ccc", typeof<string>)); ("ddd", (box 1, typeof<int>))])
    assert_equal "select * from xxx.aaa where xxx.aaa.bbb = /* bbb */'a' and xxx.aaa.ddd = /*# ddd */ order by xxx.aaa.ccc" sql

  type Hoge1 = {[<Id>]Id:int; Name:string; [<Version>]Version:int; }
  type Hoge2 = {[<Id>]Id:int; Name:string }
  type Hoge3 = {Name:string; [<Version>]Version:int}
  type Hoge4 = {Name:string}

  [<Test>]
  let ``prepareInsert : id and version`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge1> config.Dialect
    let ps = Sql.prepareInsert config { Hoge1.Id = 1; Name = "aaa" ; Version = 2; } meta (InsertOpt())
    assert_equal "insert into Hoge1 ( Id, Name, Version ) values ( @p0, @p1, @p2 )" ps.Text
    assert_equal 3 ps.Parameters.Length
    assert_equal 1 ps.Parameters.[0].Value
    assert_equal "aaa" ps.Parameters.[1].Value
    assert_equal 2 ps.Parameters.[2].Value

  [<Test>]
  let ``prepareInsert : id`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge2> config.Dialect
    let ps = Sql.prepareInsert config { Hoge2.Id = 1; Name = "aaa" } meta (InsertOpt())
    assert_equal "insert into Hoge2 ( Id, Name ) values ( @p0, @p1 )" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal 1 ps.Parameters.[0].Value
    assert_equal "aaa" ps.Parameters.[1].Value

  [<Test>]
  let ``prepareInsert : version`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge3> config.Dialect
    let ps = Sql.prepareInsert config { Hoge3.Name = "aaa"; Version = 2; } meta (InsertOpt())
    assert_equal "insert into Hoge3 ( Name, Version ) values ( @p0, @p1 )" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal "aaa" ps.Parameters.[0].Value
    assert_equal 2 ps.Parameters.[1].Value

  [<Test>]
  let ``prepareInsert`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge4> config.Dialect
    let ps = Sql.prepareInsert config { Hoge4.Name = "aaa" } meta (InsertOpt())
    assert_equal "insert into Hoge4 ( Name ) values ( @p0 )" ps.Text
    assert_equal 1 ps.Parameters.Length
    assert_equal "aaa" ps.Parameters.[0].Value

  type Hoge5 = {[<Id(IdKind.Identity)>]Id:int; Name:string option; Age:int option; Salary:decimal option; [<Version>]Version:int; }

  [<Test>]
  let ``prepareInsert : exclude null`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge5> config.Dialect
    let ps = Sql.prepareInsert config { Hoge5.Id = 1; Name = Some "aaa"; Age = None; Salary = None; Version= 10 } meta (InsertOpt(ExcludeNull = true))
    assert_equal "insert into Hoge5 ( Name, Version ) values ( @p0, @p1 )" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal "aaa" ps.Parameters.[0].Value
    assert_equal 10 ps.Parameters.[1].Value

  [<Test>]
  let ``prepareInsert : exclude`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge5> config.Dialect
    let ps = Sql.prepareInsert config { Hoge5.Id = 1; Name = Some "aaa"; Age = Some 20; Salary = Some 100M; Version= 10 } meta (InsertOpt(Exclude = ["Name"; "Salary"]))
    assert_equal "insert into Hoge5 ( Age, Version ) values ( @p0, @p1 )" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal 20 ps.Parameters.[0].Value
    assert_equal 10 ps.Parameters.[1].Value

  [<Test>]
  let ``prepareInsert : include`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge5> config.Dialect
    let ps = Sql.prepareInsert config { Hoge5.Id = 1; Name = Some "aaa"; Age = Some 20; Salary = Some 100M; Version= 10 } meta (InsertOpt(Include = ["Age"]))
    assert_equal "insert into Hoge5 ( Age, Version ) values ( @p0, @p1 )" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal 20 ps.Parameters.[0].Value
    assert_equal 10 ps.Parameters.[1].Value

  [<Test>]
  let ``prepareInsert : exclude and include`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge5> config.Dialect
    let ps = 
      Sql.prepareInsert 
        config 
        { Hoge5.Id = 1; Name = Some "aaa"; Age = Some 20; Salary = Some 100M; Version= 10 } 
        meta 
        (InsertOpt(Exclude = ["Salary"], Include = ["Age"; "Salary"]))
    assert_equal "insert into Hoge5 ( Age, Version ) values ( @p0, @p1 )" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal 20 ps.Parameters.[0].Value
    assert_equal 10 ps.Parameters.[1].Value

  type Hoge7 = {[<Id(IdKind.Identity)>]Id:int; Name:string option; [<Version(VersionKind.Computed)>]Version:int; }

  [<Test>]
  let ``prepareInsert : NoInsertablePropertyException `` () =
    let meta = Meta.makeEntityMeta typeof<Hoge7> config.Dialect
    try
      Sql.prepareInsert 
        config 
        { Hoge7.Id = 0; Name = None; Version= 0 } 
        meta 
        (InsertOpt(ExcludeNull = true)) |> ignore
      fail ()
    with
    | NoInsertablePropertyException() as ex ->
      printfn "%A" ex
    | _ -> 
      fail ()

  [<Test>]
  let ``prepareUpdate id and version`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge1> config.Dialect
    let ps = Sql.prepareUpdate config { Hoge1.Id = 1; Version = 2; Name = "aaa" } meta (UpdateOpt())
    assert_equal "update Hoge1 set Name = @p0, Version = Version + 1 where Id = @p1 and Version = @p2" ps.Text
    assert_equal 3 ps.Parameters.Length
    assert_equal "aaa" ps.Parameters.[0].Value
    assert_equal 1 ps.Parameters.[1].Value
    assert_equal 2 ps.Parameters.[2].Value

  [<Test>]
  let ``prepareUpdate id and version : ignore version`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge1> config.Dialect
    let ps = Sql.prepareUpdate config { Hoge1.Id = 1; Version = 2; Name = "aaa" } meta (UpdateOpt(IgnoreVersion = true))
    assert_equal "update Hoge1 set Name = @p0 where Id = @p1" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal "aaa" ps.Parameters.[0].Value
    assert_equal 1 ps.Parameters.[1].Value

  [<Test>]
  let ``prepareUpdate id `` () =
    let meta = Meta.makeEntityMeta typeof<Hoge2> config.Dialect
    let ps = Sql.prepareUpdate config { Hoge2.Id = 1; Name = "aaa" } meta (UpdateOpt())
    assert_equal "update Hoge2 set Name = @p0 where Id = @p1" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal "aaa" ps.Parameters.[0].Value
    assert_equal 1 ps.Parameters.[1].Value

  type Hoge6 = {[<Id(IdKind.Identity)>]Id:int; Name:string option; Age:int option; Salary:decimal option; [<Version>]Version:int; }

  [<Test>]
  let ``prepareUpdate : eclude null `` () =
    let meta = Meta.makeEntityMeta typeof<Hoge6> config.Dialect
    let ps = Sql.prepareUpdate config { Hoge6.Id = 1; Name = Some "aaa"; Age = None; Salary = None; Version= 10 } meta (UpdateOpt(ExcludeNull = true))
    assert_equal "update Hoge6 set Name = @p0, Version = Version + 1 where Id = @p1 and Version = @p2" ps.Text
    assert_equal 3 ps.Parameters.Length
    assert_equal "aaa" ps.Parameters.[0].Value
    assert_equal 1 ps.Parameters.[1].Value
    assert_equal 10 ps.Parameters.[2].Value

  [<Test>]
  let ``prepareUpdate : eclude `` () =
    let meta = Meta.makeEntityMeta typeof<Hoge6> config.Dialect
    let ps = Sql.prepareUpdate config { Hoge6.Id = 1; Name = Some "aaa"; Age = None; Salary = None; Version= 10 } meta (UpdateOpt(Exclude = ["Name"; "Salary"]))
    assert_equal "update Hoge6 set Age = @p0, Version = Version + 1 where Id = @p1 and Version = @p2" ps.Text
    assert_equal 3 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value
    assert_equal 1 ps.Parameters.[1].Value
    assert_equal 10 ps.Parameters.[2].Value

  [<Test>]
  let ``prepareUpdate : include `` () =
    let meta = Meta.makeEntityMeta typeof<Hoge6> config.Dialect
    let ps = Sql.prepareUpdate config { Hoge6.Id = 1; Name = Some "aaa"; Age = None; Salary = Some 100M; Version= 10 } meta (UpdateOpt(Include = ["Age"; "Salary"]))
    assert_equal "update Hoge6 set Age = @p0, Salary = @p1, Version = Version + 1 where Id = @p2 and Version = @p3" ps.Text
    assert_equal 4 ps.Parameters.Length
    assert_equal DBNull.Value ps.Parameters.[0].Value
    assert_equal 100M ps.Parameters.[1].Value
    assert_equal 1 ps.Parameters.[2].Value
    assert_equal 10 ps.Parameters.[3].Value

  [<Test>]
  let ``prepareUpdate : include and exclude `` () =
    let meta = Meta.makeEntityMeta typeof<Hoge6> config.Dialect
    let ps = 
      Sql.prepareUpdate 
        config 
        { Hoge6.Id = 1; Name = Some "aaa"; Age = Some 20; Salary = Some 100M; Version= 10 } 
        meta 
        (UpdateOpt(Exclude = ["Salary"], Include = ["Age"; "Salary"]))
    assert_equal "update Hoge6 set Age = @p0, Version = Version + 1 where Id = @p1 and Version = @p2" ps.Text
    assert_equal 3 ps.Parameters.Length
    assert_equal 20 ps.Parameters.[0].Value
    assert_equal 1 ps.Parameters.[1].Value
    assert_equal 10 ps.Parameters.[2].Value

  [<Test>]
  let ``prepareUpdate : NoUpdatablePropertyException `` () =
    let meta = Meta.makeEntityMeta typeof<Hoge6> config.Dialect
    try
      Sql.prepareUpdate 
        config 
        { Hoge6.Id = 1; Name = Some "aaa"; Age = Some 20; Salary = Some 100M; Version= 10 } 
        meta 
        (UpdateOpt(Exclude = ["Name"; "Age"; "Salary"], IgnoreVersion = true)) |> ignore
      fail ()
    with 
    | NoUpdatablePropertyException() as ex ->
      printfn "%A" ex.Message
    | _ -> 
      fail ()

  [<Test>]
  let ``prepareDelete : id and version`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge1> config.Dialect
    let ps = Sql.prepareDelete config { Hoge1.Id = 1; Version = 2; Name = "aaa" } meta (DeleteOpt())
    assert_equal "delete from Hoge1 where Id = @p0 and Version = @p1" ps.Text
    assert_equal 2 ps.Parameters.Length
    assert_equal 1 ps.Parameters.[0].Value
    assert_equal 2 ps.Parameters.[1].Value

  [<Test>]
  let ``prepareDelete : id and version : ignore version`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge1> config.Dialect
    let ps = Sql.prepareDelete config { Hoge1.Id = 1; Version = 2; Name = "aaa" } meta (DeleteOpt(IgnoreVersion = true))
    assert_equal "delete from Hoge1 where Id = @p0" ps.Text
    assert_equal 1 ps.Parameters.Length
    assert_equal 1 ps.Parameters.[0].Value

  [<Test>]
  let ``prepareDelete : id`` () =
    let meta = Meta.makeEntityMeta typeof<Hoge2> config.Dialect
    let ps = Sql.prepareDelete config { Hoge2.Id = 1; Name = "aaa" } meta (DeleteOpt())
    assert_equal "delete from Hoge2 where Id = @p0" ps.Text
    assert_equal 1 ps.Parameters.Length
    assert_equal 1 ps.Parameters.[0].Value

  type Color =
    | White = 0
    | Red = 1
    | Green = 2
    | Blue = 3

  [<Flags>]
  type FileAttributes =
    | ReadOnly = 0x0001
    | Hidden = 0x0002
    | System = 0x0004

  [<Test>]
  let ``MsSqlDialect : ConvertFromDbToClr : basic type`` () =
    let dialect = MsSqlDialect()
    // raw type
    assert_equal 1 (dialect.ConvertFromDbToClr(1, typeof<int>))
    assert_equal null (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<int>))
    // option type
    assert_equal (Some 1) (dialect.ConvertFromDbToClr(1, typeof<int option>))
    assert_equal None (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<int option>))
    // nullable type
    assert_equal (Nullable(1)) (dialect.ConvertFromDbToClr(1, typeof<int Nullable>))
    assert_equal (Nullable()) (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<int Nullable>))

  [<Test>]
  let ``MsSqlDialect : ConvertFromDbToClr : enum type`` () =
    let dialect = MsSqlDialect()
    // raw type
    assert_equal Color.Green (dialect.ConvertFromDbToClr(2, typeof<Color>))
    assert_equal null (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<Color>))
    assert_equal FileAttributes.Hidden (dialect.ConvertFromDbToClr(0x0002, typeof<FileAttributes>))
    assert_equal (FileAttributes.ReadOnly ||| FileAttributes.Hidden) (dialect.ConvertFromDbToClr(0x0003, typeof<FileAttributes>))
    assert_equal null (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<FileAttributes>))
    // option type
    assert_equal (Some Color.Green) (dialect.ConvertFromDbToClr(2, typeof<Color option>))
    assert_equal None (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<Color option>))
    assert_equal (Some FileAttributes.Hidden) (dialect.ConvertFromDbToClr(0x0002, typeof<FileAttributes option>))
    assert_equal (Some (FileAttributes.ReadOnly ||| FileAttributes.Hidden)) (dialect.ConvertFromDbToClr(0x0003, typeof<FileAttributes option>))
    assert_equal None (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<FileAttributes option>))
    // nullable type
    assert_equal (Nullable Color.Green) (dialect.ConvertFromDbToClr(2, typeof<Color Nullable>))
    assert_equal (Nullable()) (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<Color Nullable>))
    assert_equal (Nullable(FileAttributes.Hidden)) (dialect.ConvertFromDbToClr(0x0002, typeof<FileAttributes Nullable>))
    assert_equal (Nullable (FileAttributes.ReadOnly ||| FileAttributes.Hidden)) (dialect.ConvertFromDbToClr(0x0003, typeof<FileAttributes Nullable>))
    assert_equal (Nullable()) (dialect.ConvertFromDbToClr(Convert.DBNull, typeof<FileAttributes Nullable>))

  [<Test>]
  let ``MsSqlDialect : ConvertFromClrToDb : basic type`` () =
    let dialect = MsSqlDialect()
    // raw type
    assert_equal (box 1, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(1, typeof<int>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(null, typeof<int>))
    // option type
    assert_equal (box 1, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Some 1, typeof<int option>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(None, typeof<int option>))
    // nullable type
    assert_equal (box 1, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Nullable(1), typeof<int Nullable>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Nullable(), typeof<int Nullable>))

  [<Test>]
  let ``MsSqlDialect : ConvertFromClrToDb : enum type`` () =
    let dialect = MsSqlDialect()
    // raw type
    assert_equal (box 2, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Color.Green, typeof<Color>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(null, typeof<Color>))
    assert_equal (box 2, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(FileAttributes.Hidden, typeof<FileAttributes>))
    assert_equal (box 3, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(FileAttributes.ReadOnly ||| FileAttributes.Hidden, typeof<FileAttributes>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(null, typeof<FileAttributes>))
    // option type
    assert_equal (box 2, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Some(Color.Green), typeof<Color option>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(None, typeof<Color option>))
    assert_equal (box 2, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Some(FileAttributes.Hidden), typeof<FileAttributes option>))
    assert_equal (box 3, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Some(FileAttributes.ReadOnly ||| FileAttributes.Hidden), typeof<FileAttributes option>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(None, typeof<FileAttributes option>))
    // nullable type
    assert_equal (box 2, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Nullable(Color.Green), typeof<Color Nullable>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Nullable(), typeof<Color Nullable>))
    assert_equal (box 2, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Nullable(FileAttributes.Hidden), typeof<FileAttributes Nullable>))
    assert_equal (box 3, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Nullable(FileAttributes.ReadOnly ||| FileAttributes.Hidden), typeof<FileAttributes Nullable>))
    assert_equal (Convert.DBNull, typeof<int>, DbType.Int32) (dialect.ConvertFromClrToDb(Nullable(), typeof<FileAttributes Nullable>))

  [<Test>]
  let ``MsSqlDialect : FormatAsSqlLiteral`` () =
    let dialect = MsSqlDialect()
    assert_equal "null" (dialect.FormatAsSqlLiteral(Convert.DBNull, typeof<int>, DbType.Int32))
    assert_equal "N'aaa'" (dialect.FormatAsSqlLiteral("aaa", typeof<string>, DbType.String))
    assert_equal "'12:34:56'" (dialect.FormatAsSqlLiteral(TimeSpan(12, 34, 56), typeof<TimeSpan>, DbType.Time))
    assert_equal "'1234-01-23'" (dialect.FormatAsSqlLiteral(DateTime(1234, 01, 23), typeof<DateTime>, DbType.Date))
    assert_equal "'1234-01-23 13:34:46.123'" (dialect.FormatAsSqlLiteral(DateTime(1234, 01, 23, 13, 34, 46, 123), typeof<DateTime>, DbType.DateTime))
    let datetime = DateTime.ParseExact("1234-01-23 13:34:46.1234567", "yyyy-MM-dd HH:mm:ss.fffffff", CultureInfo.InvariantCulture)
    assert_equal "'1234-01-23 13:34:46.1234567'" (dialect.FormatAsSqlLiteral(datetime, typeof<DateTime>, DbType.DateTime2))
    let dtOffset = DateTimeOffset(1234, 01, 23, 13, 34, 46, 123, TimeSpan(1, 23, 00))
    assert_equal "'1234-01-23 13:34:46 +01:23'" (dialect.FormatAsSqlLiteral(dtOffset, typeof<DateTimeOffset>, DbType.DateTimeOffset))
    assert_equal "/** binary value is not shown */null" (dialect.FormatAsSqlLiteral([| 0uy .. 5uy |], typeof<byte[]>, DbType.Binary))
    assert_equal "'0'" (dialect.FormatAsSqlLiteral(false, typeof<bool>, DbType.Boolean))
    assert_equal "'1'" (dialect.FormatAsSqlLiteral(true, typeof<bool>, DbType.Boolean))
    assert_equal "123" (dialect.FormatAsSqlLiteral(123L, typeof<int64>, DbType.Int64))

  [<Test>]
  let ``MsSqlDialect : CreateParameterName`` () =
    let dialect = MsSqlDialect()
    assert_equal "@p0" (dialect.CreateParameterName(0))
    assert_equal "@p1" (dialect.CreateParameterName(1))

  [<Test>]
  let ``MsSqlDialect : object expression`` () =
    let dialect =
      { new MsSqlDialect() with 
        member this.RootExprCtxt = Map.empty :> IDictionary<string, obj * Type> }
    ()

  [<Test>]
  let ``MsSqlDialect : RewriteForPagination : offset is zero`` () =
    let dialect = MsSqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql, Map.empty, 0L, 10L)
    assert_equal 
      "select top (/* soma_limit */10) * from aaa where bbb = /* bbb */'a' order by ccc"
      sql
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MsSqlDialect : RewriteForPagination : offset is zero : if block`` () =
    let dialect = MsSqlDialect()
    let sql = "select * from aaa where /*% if true */bbb = /* bbb */'a' /*% elif true */ bbb = 'b' /*% else */ bbb = 'c' /*% end */ order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql, Map.empty, 0L, 10L)
    assert_equal 
      "select top (/* soma_limit */10) * from aaa where /*% if true */bbb = /* bbb */'a' /*% elif true */ bbb = 'b' /*% else */ bbb = 'c' /*% end */ order by ccc"
      sql
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MsSqlDialect : RewriteForPagination : offset is zero : for block`` () =
    let dialect = MsSqlDialect()
    let sql = 
      "select * from aaa where
      /*%for b in bbb*/
      bbb = /* b */'b' 
      /*%if bbb_has_next*/ /*#'or'*/ /*%end*/
      /*%end*/
      order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql, Map.empty, 0L, 10L)
    assert_equal
      "select top (/* soma_limit */10) * from aaa where
      /*% for b in bbb */
      bbb = /* b */'b' 
      /*% if bbb_has_next */ /*# 'or' */ /*% end */
      /*% end */
      order by ccc"
      sql
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MsSqlDialect : RewriteForPagination : offset is not zero`` () =
    let dialect = MsSqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql, Map.empty, 5L, 10L)
    assert_equal 
      "select * from ( select temp_.*, row_number() over( order by ccc ) as soma_rownumber_ from ( select * from aaa where bbb = /* bbb */'a' ) temp_ ) temp2_ where soma_rownumber_ > /* soma_offset */5 and soma_rownumber_ <= /* soma_offset + soma_limit */15"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MsSqlDialect : RewriteForPagination : offset is not zero : column qualified`` () =
    let dialect = MsSqlDialect()
    let sql = "select * from xxx.aaa where xxx.aaa.bbb = /* bbb */'a' order by xxx.aaa.ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql, Map.empty, 5L, 10L)
    assert_equal 
      "select * from ( select temp_.*, row_number() over( order by temp_.ccc ) as soma_rownumber_ from ( select * from xxx.aaa where xxx.aaa.bbb = /* bbb */'a' ) temp_ ) temp2_ where soma_rownumber_ > /* soma_offset */5 and soma_rownumber_ <= /* soma_offset + soma_limit */15"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MsSqlDialect : RewriteForPagination : offset is not zero : if block`` () =
    let dialect = MsSqlDialect()
    let sql =
      "select * from aaa where
      /*%for b in bbb*/
      bbb = /* b */'b' 
      /*%if bbb_has_next*/ /*#'or'*/ /*%end*/
      /*%end*/
      order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql, Map.empty, 5L, 10L)
    assert_equal
      "select * from ( select temp_.*, row_number() over( order by ccc ) as soma_rownumber_ from ( select * from aaa where
      /*% for b in bbb */
      bbb = /* b */'b' 
      /*% if bbb_has_next */ /*# 'or' */ /*% end */
      /*% end */
      ) temp_ ) temp2_ where soma_rownumber_ > /* soma_offset */5 and soma_rownumber_ <= /* soma_offset + soma_limit */15"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MsSqlDialect : RewriteForPagination : offset is not zero : for block`` () =
    let dialect = MsSqlDialect()
    let sql = "select * from aaa where /*% if true */ bbb = /* bbb */'a' /*% end */ order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql, Map.empty, 5L, 10L)
    assert_equal 
      "select * from ( select temp_.*, row_number() over( order by ccc ) as soma_rownumber_ from ( select * from aaa where /*% if true */ bbb = /* bbb */'a' /*% end */ ) temp_ ) temp2_ where soma_rownumber_ > /* soma_offset */5 and soma_rownumber_ <= /* soma_offset + soma_limit */15"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MsSqlDialect : RewriteForPagination : offset is not zero : no order by clause`` () =
    let dialect = MsSqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a'"
    let statement = Sql.parse sql
    try
      dialect.RewriteForPagination(statement, sql, Map.empty, 5L, 10L) |> ignore
      fail ()
    with
    | :? SqlException as ex ->
      printfn "%s" ex.Message
      assert_equal "SOMA2016" ex.MessageId
    | ex -> 
      fail ex

  [<Test>]
  let ``MsSqlDialect : RewriteForCount`` () =
    let dialect = MsSqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForCount(statement, sql,  Map.empty) 
    assert_equal 
      "select count_big(*) from ( select * from aaa where bbb = /* bbb */'a'  ) t_"
      sql
    assert_true (exprCtxt.Count = 0)

  [<Test>]
  let ``MsSqlDialect : EscapeMetaChars`` () =
    let dialect = MsSqlDialect()
    assert_equal "abc" (dialect.EscapeMetaChars "abc")
    assert_equal "ab$%c" (dialect.EscapeMetaChars "ab%c")
    assert_equal "ab$_c" (dialect.EscapeMetaChars "ab_c")
    assert_equal "ab$$c" (dialect.EscapeMetaChars "ab$c")
    assert_equal "ab$[c" (dialect.EscapeMetaChars "ab[c")

  [<Test>]
  let ``MySqlDialect : object expression`` () =
    let dialect =
      { new MySqlDialect() with 
        member this.RootExprCtxt =  Map.empty :> IDictionary<string, obj * Type> }
    ()

  [<Test>]
  let ``MySqlDialect : RewriteForPagination`` () =
    let dialect = MySqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql,  Map.empty, 3L, 10L)
    assert_equal 
      "select * from aaa where bbb = /* bbb */'a' order by ccc limit /* soma_offset */3, /* soma_limit */10"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MySqlDialect : RewriteForPagination : offset zero limit zero`` () =
    let dialect = MySqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql,  Map.empty, 0L, 0L)
    assert_equal 
      "select * from aaa where bbb = /* bbb */'a' order by ccc limit /* soma_offset */0, /* soma_limit */0"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MySqlDialect : RewriteForPagination : offset zero limit negative`` () =
    let dialect = MySqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql,  Map.empty, 0L, -1L)
    assert_equal 
      "select * from aaa where bbb = /* bbb */'a' order by ccc limit /* soma_offset */0, /* soma_limit */18446744073709551615"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MySqlDialect : RewriteForPagination : for update`` () =
    let dialect = MySqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc for update"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForPagination(statement, sql,  Map.empty, 3L, 10L)
    assert_equal 
      "select * from aaa where bbb = /* bbb */'a' order by ccc  limit /* soma_offset */3, /* soma_limit */10 for update"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MySqlDialect : RewriteForCalcPagination`` () =
    let dialect = MySqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForCalcPagination(statement, sql,  Map.empty, 3L, 10L)
    assert_equal 
      "select sql_calc_found_rows * from aaa where bbb = /* bbb */'a' order by ccc limit /* soma_offset */3, /* soma_limit */10"
      sql
    assert_true (exprCtxt.ContainsKey "soma_offset")
    assert_true (exprCtxt.ContainsKey "soma_limit")

  [<Test>]
  let ``MySqlDialect : RewriteForCount`` () =
    let dialect = MySqlDialect()
    let sql = "select * from aaa where bbb = /* bbb */'a' order by ccc"
    let statement = Sql.parse sql
    let sql, exprCtxt = dialect.RewriteForCount(statement, sql,  Map.empty) 
    assert_equal 
      "select found_rows()"
      sql
    assert_true (exprCtxt.Count = 0)

  [<Test>]
  let ``MySqlDialect : EscapeMetaChars`` () =
    let dialect = MySqlDialect()
    assert_equal "abc" (dialect.EscapeMetaChars "abc")
    assert_equal "ab$%c" (dialect.EscapeMetaChars "ab%c")
    assert_equal "ab$_c" (dialect.EscapeMetaChars "ab_c")
    assert_equal "ab$$c" (dialect.EscapeMetaChars "ab$c")

  [<Test>]
  let ``SQLiteDialect : ConvertFromDbToUnderlyingClr`` () =
    let dialect = SQLiteDialect()
    let guid = Guid()
    let value = dialect.ConvertFromDbToUnderlyingClr(guid.ToByteArray(), typeof<Guid>)
    assert_equal guid value

  [<Test>]
  let ``function : illegal argument type`` () =
    let dialect =
      { new MsSqlDialect() with 
        member this.RootExprCtxt =  
          let dict = new Dictionary<string, (obj * Type)>(base.RootExprCtxt) :> IDictionary<string, (obj * Type)>
          let hoge s = s + "hoge"
          dict.Add("hoge", (box hoge, hoge.GetType()))
          dict } :> IDialect
    let config = 
      { new MsSqlConfig() with
        member this.ConnectionString = ""
        member this.Dialect = dialect }
    try
      Sql.prepare config "select * from aaa where /*% if hoge(bbb) */ bbb = /* bbb */'a'/*% end*/" (dict ["bbb", (box 10, typeof<int>)]) parser |> ignore
      fail ()
    with 
    | :? SqlException as ex->
      printf "%s" ex.Message
      assert_equal "SOMA2007" ex.MessageId
      assert_true (ex.Message.Contains "[SOMA1024]")

  [<Test>]
  let ``function : invocation failed`` () =
    let dialect =
      { new MsSqlDialect() with 
        member this.RootExprCtxt =  
          let dict = new Dictionary<string, (obj * Type)>(base.RootExprCtxt) :> IDictionary<string, (obj * Type)>
          let hoge s:string = raise <| invalidOp "hoge is invalid"
          dict.Add("hoge", (box hoge, hoge.GetType()))
          dict } :> IDialect
    let config = 
      { new MsSqlConfig() with
        member this.ConnectionString = ""
        member this.Dialect = dialect }
    try
      Sql.prepare config "select * from aaa where /*% if hoge(bbb) */ bbb = /* bbb */'a'/*% end*/" (dict ["bbb", (box "foo", typeof<string>)]) parser |> ignore
      fail ()
    with 
    | :? SqlException as ex->
      printf "%s" ex.Message
      assert_equal "SOMA2007" ex.MessageId
      assert_true (ex.Message.Contains "[SOMA1025]")