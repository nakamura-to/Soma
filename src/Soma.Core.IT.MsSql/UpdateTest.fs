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

module UpdateTest = 

  [<Test>]
  let ``update : no id``() =
    use ts = new TransactionScope()
    try
      MsSql.update { NoId.Name = "aaa"; VersionNo = 0 } |> ignore
      fail ()
    with 
    | :? InvalidOperationException as ex -> 
      assert_true <| ex.Message.StartsWith "[SOMA4005]"
      printfn "%A" ex
    | ex -> 
      fail ex

  [<Test>]
  let ``update : composite id``() =
    use ts = new TransactionScope()
    let employee = MsSql.find<CompKeyEmployee> [2; 12]
    let employee = MsSql.update { employee with EmployeeName = "hoge" }
    printfn "%A" employee
    assert_equal { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "hoge"; VersionNo = 1; } employee

  [<Test>]
  let ``update : no version``() =
    use ts = new TransactionScope()
    let noVersion = MsSql.update { NoVersion.Id = 1; Name = "aaa" }
    printfn "%A" noVersion
    assert_equal { NoVersion.Id = 1; Name = "aaa" } noVersion

  [<Test>]
  let ``update : incremented version``() =
    use ts = new TransactionScope()
    let department = { DepartmentId = 1; DepartmentName = "hoge"; VersionNo = 0 }
    let department = MsSql.update department
    printfn "%A" department
    assert_equal 1 department.VersionNo

  [<Test>]
  let ``update : computed version``() =
    use ts = new TransactionScope()
    let address = MsSql.insert { Address.AddressId = 0; Street = "hoge"; VersionNo = Array.empty }
    let address = { address with Street = "foo" }
    let version = address.VersionNo
    let address = MsSql.update address
    printfn "%A" address
    assert_true (version <> address.VersionNo)

  [<Test>]
  let ``update : Enum``() =
    use ts = new TransactionScope()
    let person = MsSql.find<Person> [2]
    let person = MsSql.update { person with JobKind = JobKind.Salesman }
    printfn "%A" person
    assert_equal { PersonId = 2; PersonName = "Martin"; JobKind = JobKind.Salesman; VersionNo = 1; } person


  [<Test>]
  let ``update : unique constraint violation``() =
    use ts = new TransactionScope()
    let department = { DepartmentId = 1; DepartmentName = "Sales"; VersionNo = 0 }
    try
      MsSql.update department |> ignore
      fail ()
    with 
    | UniqueConstraintException _ as ex -> printfn "%A" ex
    | ex -> fail ex

  [<Test>]
  let ``update : optimistic lock confliction``() =
    use ts = new TransactionScope()
    let department = { DepartmentId = 1; DepartmentName = "hoge"; VersionNo = -1 }
    try
      MsSql.update department |> ignore
      fail ()
    with 
    | OptimisticLockException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex

  [<Test>]
  let ``updateIgnoreVersion``() =
    use ts = new TransactionScope()
    let department = { DepartmentId = 1; DepartmentName = "hoge"; VersionNo = -1 }
    let department = MsSql.updateWithOpt department (UpdateOpt(IgnoreVersion = true))
    printfn "%A" department

  [<Test>]
  let ``updateIgnoreVersion : no affected row``() =
    use ts = new TransactionScope()
    let department = { DepartmentId = 0; DepartmentName = "hoge"; VersionNo = -1 }
    try
      MsSql.updateWithOpt department (UpdateOpt(IgnoreVersion = true)) |> ignore
      fail ()
    with 
    | NoAffectedRowException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex

  [<Test>]
  let ``update : exclude null``() =
    use ts = new TransactionScope()
    let employee = MsSql.find<Employee> [1]
    let employee = MsSql.updateWithOpt { employee with EmployeeName = None} (UpdateOpt(ExcludeNull = true))
    assert_equal { EmployeeId = Some 1; EmployeeName = None; DepartmentId = Some 1; VersionNo = Some 1; } employee

  [<Test>]
  let ``update : exclude``() =
    use ts = new TransactionScope()
    let employee = MsSql.find<Employee> [1]
    let employee = MsSql.updateWithOpt { employee with EmployeeName = Some "hoge"; DepartmentId = Some 2} (UpdateOpt(Exclude = ["EmployeeName"]))
    let employee = MsSql.find<Employee> [1]
    assert_equal { EmployeeId = Some 1; EmployeeName = Some "King"; DepartmentId = Some 2; VersionNo = Some 1; } employee

  [<Test>]
  let ``update : include``() =
    use ts = new TransactionScope()
    let employee = MsSql.find<Employee> [1]
    let employee = MsSql.updateWithOpt { employee with EmployeeName = Some "hoge"; DepartmentId = Some 2} (UpdateOpt(Include = ["EmployeeName"]))
    let employee = MsSql.find<Employee> [1]
    assert_equal { EmployeeId = Some 1; EmployeeName = Some "hoge"; DepartmentId = Some 1; VersionNo = Some 1; } employee