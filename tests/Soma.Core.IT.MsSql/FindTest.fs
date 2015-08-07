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

  [<Test>]
  let ``find : by empty id``() =
    try
      MsSql.find<NoId> [] |> ignore
      fail ()
    with
    | :? InvalidOperationException as ex -> 
      assert_true <| ex.Message.StartsWith "[SOMA4004]"
      printfn "%A" ex
    | ex -> 
      fail ex

  [<Test>]
  let ``find : by id for no id record``() =
    try
      MsSql.find<NoId> [2] |> ignore
      fail ()
    with
    | :? InvalidOperationException as ex -> 
      assert_true <| ex.Message.StartsWith "[SOMA4005]"
      printfn "%A" ex
    | ex -> 
      fail ex

  [<Test>]
  let ``find``() =
    let department = MsSql.find<Department> [2]
    printfn "%A" department
    assert_equal { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; } department

  [<Test>]
  let ``find : by composite id``() =
    let employee = MsSql.find<CompKeyEmployee> [2; 12]
    printfn "%A" employee
    assert_equal { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; } employee

  [<Test>]
  let ``find : id type is option``() =
    let employee = MsSql.find<Employee> [1]
    printfn "%A" employee

  [<Test>]
  let ``find : not found``() =
    try
      MsSql.find<Department> [99] |> ignore
      fail ()
    with 
    | EntityNotFoundException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex

  [<Test>]
  let ``tryFind : id``() =
    let department = MsSql.tryFind<Department> [2]
    printfn "%A" department
    assert_equal (Some { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; }) department

  [<Test>]
  let ``tryFind : by composite id``() =
    let employee = MsSql.tryFind<CompKeyEmployee> [2; 12]
    printfn "%A" employee
    assert_equal (Some { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; }) employee

  [<Test>]
  let ``tryFind : not found``() =
    let department = MsSql.tryFind<Department> [99]
    printfn "%A" department
    assert_true department.IsNone

  [<Test>]
  let ``findWithVersion``() =
    let department = MsSql.findWithVersion<Department> [2] 0
    printfn "%A" department
    assert_equal { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; } department

  [<Test>]
  let ``findWithVersion : by composite id``() =
    let employee = MsSql.findWithVersion<CompKeyEmployee> [2; 12] 0
    printfn "%A" employee
    assert_equal { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; } employee

  [<Test>]
  let ``findWithVersion : not found``() =
    try
      MsSql.findWithVersion<Department> [99] 0 |> ignore
      fail ()
    with
    | EntityNotFoundException _ as ex -> printfn "%A" ex

  [<Test>]
  let ``findWithVersion : optimistic lock confliction``() =
    try
      MsSql.findWithVersion<Department> [2] 99 |> ignore
      fail ()
    with
    | OptimisticLockException _ as ex -> printfn "%A" ex

  [<Test>]
  let ``tryFindWithVersion``() =
    let department = MsSql.tryFindWithVersion<Department> [2] 0
    printfn "%A" department
    assert_equal (Some { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; }) department

  [<Test>]
  let ``tryFindWithVersion : by composite id``() =
    let employee = MsSql.tryFindWithVersion<CompKeyEmployee> [2; 12] 0
    printfn "%A" employee
    assert_equal (Some { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; }) employee

  [<Test>]
  let ``tryFindWithVersion : not found``() =
    let department = MsSql.tryFindWithVersion<Department> [99] 0
    assert_true department.IsNone

  [<Test>]
  let ``tryFindWithVersion : optimistic lock confliction``() =
    try
      MsSql.tryFindWithVersion<Department> [2] 99 |> ignore
      fail ()
    with
    | OptimisticLockException _ as ex -> printfn "%A" ex
