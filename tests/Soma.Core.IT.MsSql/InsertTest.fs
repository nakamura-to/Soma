﻿//----------------------------------------------------------------------------
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

  [<Test>]
  let ``insert : assigned id``() =
    use ts = new TransactionScope()
    let department = { DepartmentId = 99; DepartmentName = "aaa"; VersionNo = 0 }
    let department = MsSql.insert department
    printfn "%A" department
    assert_equal 99 department.DepartmentId
    assert_equal 1 department.VersionNo

  [<Test>]
  let ``insert : assigned composite id``() =
    use ts = new TransactionScope()
    let employee = { EmployeeId1 = 99; EmployeeId2 = 1; EmployeeName = "aaa"; VersionNo = 0 }
    let employee = MsSql.insert employee
    printfn "%A" employee
    assert_equal 99 employee.EmployeeId1
    assert_equal 1 employee.EmployeeId2
    assert_equal 1 employee.VersionNo

  [<Test>]
  let ``insert : identity id``() =
    use ts = new TransactionScope()
    let employee = { Employee.EmployeeId = None; EmployeeName = Some "hoge"; DepartmentId = Some 1; VersionNo = Some 0 }
    let employee = MsSql.insert employee
    printfn "%A" employee
    assert_true employee.EmployeeId.IsSome
    assert_equal 1 employee.VersionNo.Value

  [<Test>]
  let ``insert : no id``() =
    use ts = new TransactionScope()
    let noId = { NoId.Name = "aaa"; VersionNo = 0 }
    let noId = MsSql.insert noId
    printfn "%A" noId
    assert_equal { NoId.Name = "aaa"; VersionNo = 0 } noId

  [<Test>]
  let ``insert : no version``() =
    use ts = new TransactionScope()
    let noVersion = { NoVersion.Id = 99; Name = "aaa" }
    let noVersion = MsSql.insert noVersion
    printfn "%A" noVersion
    assert_equal { NoVersion.Id = 99; Name = "aaa" } noVersion

  [<Test>]
  let ``insert : unique constraint violation : unique key``() =
    use ts = new TransactionScope()
    let department = { DepartmentId = 1; DepartmentName = "aaa"; VersionNo = 0 }
    try
      MsSql.insert department |> ignore
      fail ()
    with 
    | UniqueConstraintException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex

  [<Test>]
  let ``insert : unique constraint violation : unique index``() =
    use ts = new TransactionScope()
    let person = { PersonId = 0; PersonName = "Scott"; JobKind = JobKind.Salesman; VersionNo = 0 }
    try
      MsSql.insert person |> ignore
      fail ()
    with 
    | UniqueConstraintException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex

  [<Test>]
  let ``insert : incremented version``() =
    use ts = new TransactionScope()
    let employee = { Employee.EmployeeId = None; EmployeeName = None; DepartmentId = Some 1; VersionNo = None }
    let employee = MsSql.insert employee
    printfn "%A" employee
    assert_true employee.VersionNo.IsSome
    assert_equal 1 employee.VersionNo.Value

  [<Test>]
  let ``insert : computed version``() =
    use ts = new TransactionScope()
    let address = { Address.AddressId = 0; Street = "hoge"; VersionNo = Array.empty }
    let address = MsSql.insert address
    printfn "%A" address
    assert_true <| not (Array.isEmpty address.VersionNo)

  [<Test>]
  let ``insert : exclude null``() =
    use ts = new TransactionScope()
    let employee = { Employee.EmployeeId = None; EmployeeName = None; DepartmentId = None; VersionNo = Some 0 }
    MsSql.insertWithOpt employee (InsertOpt(ExcludeNull = true)) |> ignore

  [<Test>]
  let ``insert : exclude``() =
    use ts = new TransactionScope()
    let employee = { Employee.EmployeeId = None; EmployeeName = Some "hoge"; DepartmentId = Some 1; VersionNo = Some 0 }
    let employee = MsSql.insertWithOpt employee (InsertOpt(Exclude = ["EmployeeName"]))
    let employee = MsSql.find<Employee> [employee.EmployeeId]
    assert_true employee.EmployeeName.IsNone
    assert_equal (Some 1) employee.DepartmentId

  [<Test>]
  let ``insert : include``() =
    use ts = new TransactionScope()
    let employee = { Employee.EmployeeId = None; EmployeeName = Some "hoge"; DepartmentId = Some 1; VersionNo = Some 0 }
    let employee = MsSql.insertWithOpt employee (InsertOpt(Include = ["EmployeeName"]))
    let employee = MsSql.find<Employee> [employee.EmployeeId]
    assert_true employee.EmployeeId.IsSome
    assert_equal None employee.DepartmentId