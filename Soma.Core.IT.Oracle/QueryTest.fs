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
open System.Data
open System.Data.Common
open System.Transactions
open NUnit.Framework
open Soma.Core

module QueryTest = 

  [<Test>]
  let ``select with ADO.NET fashion``() =
    let factory = Oracle.config.DbProviderFactory
    use connection = factory.CreateConnection()
    connection.ConnectionString <- Oracle.config.ConnectionString
    connection.Open()
    use command = connection.CreateCommand()
    command.CommandText <- "select DepartmentId, DepartmentName from Department"
    let records = ResizeArray() 
    use reader = command.ExecuteReader()
    if reader.HasRows then
      while reader.Read() do
        printfn "%A %A" (reader.GetValue(0)) (reader.GetValue(1))

  type Department =
    { [<Id>]
      DepartmentId : int
      DepartmentName : string
      [<Version>]
      VersionNo : int }

  type Employee =
    { [<Id>]
      EmployeeId : int option
      EmployeeName : string option
      DepartmentId : int option
      [<Version>]
      VersionNo : int option }

  type JobKind =
    | Salesman = 0
    | Manager = 1

  type Person =
    { [<Id>]
      PersonId : int
      PersonName : string
      JobKind : JobKind
      [<Version>]
      VersionNo : int }

  [<Test>]
  let ``query : 1 record``() =
    let departments = 
      Oracle.query<Department> "
        select * from Department where DepartmentId = /* id */0
        " ["id" @= 2]
    departments |> List.iter (printfn "%A")
    assert_equal 1 departments.Length
    assert_equal { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; } departments.[0]

  [<Test>]
  let ``query : 1 record by Enum value``() =
    let persons = 
      Oracle.query<Person> "
        select * from Person where JobKind = /* jobKind */0
        " ["jobKind" @= JobKind.Manager] 
    persons |> List.iter (printfn "%A")
    assert_equal 1 persons.Length
    assert_equal { PersonId = 2; PersonName = "Martin"; JobKind = JobKind.Manager; VersionNo = 0; } persons.Head

  [<Test>]
  let ``query : all records``() =
    let departments = 
      Oracle.query<Department> "
        select * from Department order by DepartmentId
        " []
    departments |> Seq.iter (printfn "%A")
    assert_equal 2 departments.Length
    assert_equal 
      [{ DepartmentId = 1; DepartmentName = "Account"; VersionNo = 0; };
       { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; }] departments

  [<Test>]
  let ``query : 1 record: option``() =
    let employees = 
      Oracle.query<Employee> "
        select * from Employee where EmployeeId = 4
        " [] 
    employees |> Seq.iter (printfn "%A")

  [<Test>]
  let ``query : using if expression comment``() =
    let greaterThanZero id = id > 1
    let departments = 
      Oracle.query<Department> "
        select * from Department where 
        /*% if greaterThanZero id */ 
          DepartmentId = /* id */0 
        /*% end */
        " ["id" @= 2; "greaterThanZero" @= greaterThanZero] 
    departments |> List.iter (printfn "%A")
    assert_equal 1 departments.Length
    assert_equal { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; } departments.[0]

  [<Test>]
  let ``query : using in operation``() =
    let id = [1; 2]
    let departments = 
      Oracle.query<Department> "
        select * from Department where DepartmentId in /* id */(10, 20)
        " ["id" @= id] 
    departments |> List.iter (printfn "%A")
    assert_equal 2 departments.Length
    assert_equal 
      [{ DepartmentId = 1; DepartmentName = "Account"; VersionNo = 0; };
       { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; }] departments

  [<Test>]
  let ``query : 1 tuple``() =
    let departments = 
      Oracle.query<int * string * int> "
        select DepartmentId, DepartmentName, VersionNo from Department where DepartmentId = /* id */0
        " ["id" @= 2] 
    departments |> List.iter (printfn "%A")
    assert_equal 1 departments.Length
    assert_equal (2, "Sales", 0) departments.[0]

  [<Test>]
  let ``query : all tuples``() =
    let departments = 
      Oracle.query<int * string * int> "
        select * from Department
        " []
    departments |> List.iter (printfn "%A")
    assert_equal 2 departments.Length

  [<Test>]
  let ``query : 1 tuple : record mixed``() =
    let departments = 
      Oracle.query<string * Employee> "
        select d.DepartmentName, e.* from Department d inner join Employee e on (d.DepartmentId = e.DepartmentId) where d.DepartmentId = /* id */0
        " ["id" @= 2] 
    departments |> List.iter (printfn "%A")

  [<Test>]
  let ``query : 1 single``() =
    let names = 
      Oracle.query<string> "
        select DepartmentName from Department where DepartmentId = /* id */0
        " ["id" @= 2]
    names |> List.iter (printfn "%A")
    assert_equal 1 names.Length
    assert_equal "Sales" names.[0]

  [<Test>]
  let ``queryOnDemand``() =
    let departments = 
      Oracle.queryOnDemand<Department> "
        select * from Department order by DepartmentId
        " []
    departments |> Seq.iter (printfn "%A")
    let departments = Seq.toList departments
    assert_equal 2 departments.Length
    assert_equal 
      [{ DepartmentId = 1; DepartmentName = "Account"; VersionNo = 0; };
       { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; }] departments

