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
open System.Configuration
open System.Data
open System.Data.Common
open System.Transactions
open NUnit.Framework
open Soma.Core

module InsertTest = 

  type Department =
    { [<Id>]
      DepartmentId : int
      DepartmentName : string
      [<Version>]
      VersionNo : int }

  type Employee =
    { [<Id(IdKind.Identity)>]
      EmployeeId : int option
      EmployeeName : string option
      DepartmentId : int option
      [<Version>]
      VersionNo : int option }

  type CompKeyEmployee =
    { [<Id>]
      EmployeeId1 : int
      [<Id>]
      EmployeeId2 : int
      EmployeeName : string
      [<Version>]
      VersionNo : int }

  type NoId =
    { Name : string
      VersionNo : int }

  type NoVersion =
    { [<Id>]
      Id : int
      Name : string }

  [<Test>]
  let ``insert : assigned id``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let department = { DepartmentId = 99; DepartmentName = "aaa"; VersionNo = 0 }
    let department = SQLite.insert con department
    printfn "%A" department
    assert_equal 99 department.DepartmentId
    assert_equal 1 department.VersionNo

  [<Test>]
  let ``insert : assigned composite id``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let employee = { EmployeeId1 = 99; EmployeeId2 = 1; EmployeeName = "aaa"; VersionNo = 0 }
    let employee = SQLite.insert con employee
    printfn "%A" employee
    assert_equal 99 employee.EmployeeId1
    assert_equal 1 employee.EmployeeId2
    assert_equal 1 employee.VersionNo

  [<Test>]
  let ``insert : identity id``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let employee = { Employee.EmployeeId = None; EmployeeName = Some "hoge"; DepartmentId = Some 1; VersionNo = Some 0 }
    let employee = SQLite.insert con employee
    printfn "%A" employee
    assert_true employee.EmployeeId.IsSome
    assert_equal 1 employee.VersionNo.Value

  [<Test>]
  let ``insert : no id``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let noId = { NoId.Name = "aaa"; VersionNo = 0 }
    let noId = SQLite.insert con noId
    printfn "%A" noId
    assert_equal { NoId.Name = "aaa"; VersionNo = 0 } noId

  [<Test>]
  let ``insert : no version``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let noVersion = { NoVersion.Id = 99; Name = "aaa" }
    let noVersion = SQLite.insert con noVersion
    printfn "%A" noVersion
    assert_equal { NoVersion.Id = 99; Name = "aaa" } noVersion

  [<Test>]
  let ``insert : unique constraint violation``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let department = { DepartmentId = 1; DepartmentName = "aaa"; VersionNo = 0 }
    try
      SQLite.insert con department |> ignore
      fail ()
    with 
    | UniqueConstraintException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex

  [<Test>]
  let ``insert : incremented version``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let employee = { Employee.EmployeeId = None; EmployeeName = None; DepartmentId = Some 1; VersionNo = None }
    let employee = SQLite.insert con employee
    printfn "%A" employee
    assert_true employee.VersionNo.IsSome
    assert_equal 1 employee.VersionNo.Value

