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

open System
open System.Transactions
open Soma.Core

// define a module wraps Soma.Core.Db module
module MyDb = 
  let config = 
    { new MsSqlConfig() with
      member this.ConnectionString = "Data Source=.\SQLEXPRESS;Initial Catalog=Soma.Tutorial;Integrated Security=True" }
  let query<'T> = Db.query<'T> config
  let queryOnDemand<'T> = Db.queryOnDemand<'T> config
  let execute sql expr = Db.execute config sql expr
  let find<'T when 'T : not struct> = Db.find<'T> config
  let tryFind<'T when 'T : not struct> = Db.tryFind<'T> config
  let insert<'T when 'T : not struct> = Db.insert<'T> config
  let update<'T when 'T : not struct> = Db.update<'T> config
  let delete<'T when 'T : not struct> = Db.delete<'T> config
  let call<'T when 'T : not struct> = Db.call<'T> config

// define a record mapped to a table 
type Employee = 
  { [<Id(IdKind.Identity)>]
    EmployeeId : int 
    EmployeeName : string
    DepartmentId : int
    [<Version>]
    VersionNo : int }

// define a record mapped to a procedure
type ProcResultAndOut = 
  { EmployeeId : int
    [<ProcedureParam(Direction = Direction.Output)>]
    EmployeeCount : int
    [<ProcedureParam(Direction = Direction.Result)>]
    Employees : Employee list }

let main =
  // execute following code in a transaction, but don't commit
  use tx = new TransactionScope()
  
  // find by id
  let emp = MyDb.find<Employee> [1]
  printfn "FOUND RECORD : \n%A\n" emp

  // update
  let emp = MyDb.update { emp with EmployeeName = "Hoge" }
  printfn "UPDATED RECORD : \n%A\n" emp

  // delete
  MyDb.delete emp
  printfn "DELETED RECORD : \n%A\n" emp

  // insert
  let emp = MyDb.insert { EmployeeId = 0; EmployeeName = "Allen"; DepartmentId = 2; VersionNo = 0}
  printfn "INSERTED RECORD : \n%A\n" emp

  // query and map results to records. parameters are bindable with the list of key/value pair. 
  let empList = 
    MyDb.query<Employee> @"
    select 
      e.EmployeeId, e.EmployeeName, e.DepartmentId, e.VersionNo   
    from 
      Employee e 
    where 
      e.DepartmentId = /* emp.DepartmentId */0 
    " ["emp" @= emp]
  printfn "QUERY RESULTS AS RECORDS :"
  empList |> List.iter (printfn "%A")
  printfn ""

  // query and map results to dynamic objects. parameters are bindable with the list of key/value pair. 
  let empList = 
    MyDb.query<dynamic> @"
    select 
      e.EmployeeId, e.EmployeeName, e.DepartmentId, e.VersionNo   
    from 
      Employee e 
    where 
      e.DepartmentId = /* emp.DepartmentId */0 
    " ["emp" @= emp]
  printfn "QUERY RESULTS AS DYNAMIC OBJECTS :"
  empList |> List.iter (fun emp -> printfn "EmployeeId=%O, EmployeeName=%O" emp?EmployeeId emp?EmployeeName)
  printfn ""

  // call procedure
  let result = MyDb.call<ProcResultAndOut> { EmployeeId = 1; EmployeeCount = 0; Employees = [] }
  printfn "PROCEDURE OUTPUT : \n%A\n" result

  // exequte arbitrary SQL
  let rows = 
    MyDb.execute @"
    delete from Employee 
    " []
  printfn "AFFECTED ROWS : \n%A\n" rows

  Console.ReadKey()