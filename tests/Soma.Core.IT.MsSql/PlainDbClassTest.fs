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

namespace Soma.Core.IT

open System
open System.Collections
open System.Data
open System.Data.Common
open System.Transactions
open NUnit.Framework
open Soma.Core

module PlainDbClassTest = 

  let db = PlainDb(MsSql.config)

  [<Test>]
  let ``query : 1 record``() =
    let condition = Hashtable()
    condition.["id"] <- 2
    let departments = 
      db.Query (
        "select * from Department where DepartmentId = /* id */0", 
        condition)
    departments 
    |> Seq.iter (fun department ->
      for entry in department do
        match entry with
        | :? DictionaryEntry as entry -> printfn "%A, %A" entry.Key entry.Value
        | _-> () )
    assert_equal 1 departments.Count
    let department = departments.[0]
    assert_equal 2 department.["DepartmentId"]
    assert_equal "Sales" department.["DepartmentName"]
    assert_equal 0 department.["VersionNo"]

  [<Test>]
  let ``execute``() =
    use tx = new TransactionScope ()
    let employee = Hashtable()
    employee.["EmployeeName"] <- "hoge"
    employee.["VersionNo"] <- 99
    let rows = db.Execute("insert into Employee (EmployeeName, VersionNo) values (/*EmployeeName*/'x', /*VersionNo*/0)", employee)
    assert_equal 1 rows

