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

module FindTest = 

  type Department =
    { [<Id>]
      DepartmentId : int
      DepartmentName : string
      [<Version>]
      VersionNo : int }

  [<Table(IsEnclosed = true)>]
  type Employee =
    { [<Id(IdKind.Identity)>]
      EmployeeId : int option
      [<Column(IsEnclosed = true)>]
      EmployeeName : string option
      DepartmentId : int option
      [<Version>]
      VersionNo : int option }

  type Address =
    { [<Id(IdKind.Identity)>]
      AddressId : int
      Street : string
      [<Version>]
      VersionNo : byte array }

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
  let ``find : by empty id``() =
    use con = MsSqlCe.createConnection()
    try
      MsSqlCe.find<NoId> con [] |> ignore
      fail ()
    with
    | :? InvalidOperationException as ex -> 
      assert_true <| ex.Message.StartsWith "[SOMA4004]"
      printfn "%A" ex
    | ex -> 
      fail ex

  [<Test>]
  let ``find : by id for no id record``() =
    use con = MsSqlCe.createConnection()
    try
      MsSqlCe.find<NoId> con [2] |> ignore
      fail ()
    with
    | :? InvalidOperationException as ex -> 
      assert_true <| ex.Message.StartsWith "[SOMA4005]"
      printfn "%A" ex
    | ex -> 
      fail ex

  [<Test>]
  let ``find``() =
    use con = MsSqlCe.createConnection()
    let department = MsSqlCe.find<Department> con [2]
    printfn "%A" department
    assert_equal { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; } department

  [<Test>]
  let ``find : by composite id``() =
    use con = MsSqlCe.createConnection()
    let employee = MsSqlCe.find<CompKeyEmployee> con [2; 12]
    printfn "%A" employee
    assert_equal { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; } employee

  [<Test>]
  let ``find : id type is option``() =
    use con = MsSqlCe.createConnection()
    let employee = MsSqlCe.find<Employee> con [1]
    printfn "%A" employee

  [<Test>]
  let ``find : not found``() =
    use con = MsSqlCe.createConnection()
    try
      MsSqlCe.find<Department> con [99] |> ignore
      fail ()
    with 
    | EntityNotFoundException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex

  [<Test>]
  let ``tryFind : id``() =
    use con = MsSqlCe.createConnection()
    let department = MsSqlCe.tryFind<Department> con [2]
    printfn "%A" department
    assert_equal (Some { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; }) department

  [<Test>]
  let ``tryFind : by composite id``() =
    use con = MsSqlCe.createConnection()
    let employee = MsSqlCe.tryFind<CompKeyEmployee> con [2; 12]
    printfn "%A" employee
    assert_equal (Some { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; }) employee

  [<Test>]
  let ``tryFind : not found``() =
    use con = MsSqlCe.createConnection()
    let department = MsSqlCe.tryFind<Department> con [99]
    printfn "%A" department
    assert_true department.IsNone

  [<Test>]
  let ``findWithVersion``() =
    use con = MsSqlCe.createConnection()
    let department = MsSqlCe.findWithVersion<Department> con [2] 0
    printfn "%A" department
    assert_equal { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; } department

  [<Test>]
  let ``findWithVersion : by composite id``() =
    use con = MsSqlCe.createConnection()
    let employee = MsSqlCe.findWithVersion<CompKeyEmployee> con [2; 12] 0
    printfn "%A" employee
    assert_equal { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; } employee

  [<Test>]
  let ``findWithVersion : not found``() =
    use con = MsSqlCe.createConnection()
    try
      MsSqlCe.findWithVersion<Department> con [99] 0 |> ignore
      fail ()
    with
    | EntityNotFoundException _ as ex -> printfn "%A" ex

  [<Test>]
  let ``findWithVersion : optimistic lock confliction``() =
    use con = MsSqlCe.createConnection()
    try
      MsSqlCe.findWithVersion<Department> con [2] 99 |> ignore
      fail ()
    with
    | OptimisticLockException _ as ex -> printfn "%A" ex

  [<Test>]
  let ``tryFindWithVersion``() =
    use con = MsSqlCe.createConnection()
    let department = MsSqlCe.tryFindWithVersion<Department> con [2] 0
    printfn "%A" department
    assert_equal (Some { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; }) department

  [<Test>]
  let ``tryFindWithVersion : by composite id``() =
    use con = MsSqlCe.createConnection()
    let employee = MsSqlCe.tryFindWithVersion<CompKeyEmployee> con [2; 12] 0
    printfn "%A" employee
    assert_equal (Some { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; }) employee

  [<Test>]
  let ``tryFindWithVersion : not found``() =
    use con = MsSqlCe.createConnection()
    let department = MsSqlCe.tryFindWithVersion<Department> con [99] 0
    assert_true department.IsNone

  [<Test>]
  let ``tryFindWithVersion : optimistic lock confliction``() =
    use con = MsSqlCe.createConnection()
    try
      MsSqlCe.tryFindWithVersion<Department> con [2] 99 |> ignore
      fail ()
    with
    | OptimisticLockException _ as ex -> printfn "%A" ex
