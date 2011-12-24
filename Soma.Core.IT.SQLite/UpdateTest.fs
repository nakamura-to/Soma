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

  type JobKind =
    | Salesman = 0
    | Manager = 1

  type Person =
    { [<Id(IdKind.Identity)>]
      PersonId : int
      PersonName : string
      JobKind : JobKind
      [<Version>]
      VersionNo : int }

  [<Test>]
  let ``update : no id``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    try
      SQLite.update con { NoId.Name = "aaa"; VersionNo = 0 } |> ignore
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
    use con = SQLite.createConnection()
    let employee = SQLite.find<CompKeyEmployee> con [2; 12]
    let employee = SQLite.update con { employee with EmployeeName = "hoge" }
    printfn "%A" employee
    assert_equal { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "hoge"; VersionNo = 1; } employee

  [<Test>]
  let ``update : no version``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let noVersion = SQLite.update con { NoVersion.Id = 1; Name = "aaa" }
    printfn "%A" noVersion
    assert_equal { NoVersion.Id = 1; Name = "aaa" } noVersion

  [<Test>]
  let ``update : incremented version``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let department = { DepartmentId = 1; DepartmentName = "hoge"; VersionNo = 0 }
    let department = SQLite.update con department
    printfn "%A" department
    assert_equal 1 department.VersionNo

  [<Test>]
  let ``update : Enum``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let person = SQLite.find<Person> con [2]
    let person = SQLite.update con { person with JobKind = JobKind.Salesman }
    printfn "%A" person
    assert_equal { PersonId = 2; PersonName = "Martin"; JobKind = JobKind.Salesman; VersionNo = 1; } person

  [<Test>]
  let ``update : unique constraint violation``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let department = { DepartmentId = 1; DepartmentName = "Sales"; VersionNo = 0 }
    try
      SQLite.update con department |> ignore
      fail ()
    with 
    | UniqueConstraintException _ as ex -> printfn "%A" ex
    | ex -> fail ex

  [<Test>]
  let ``update : optimistic lock confliction``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let department = { DepartmentId = 1; DepartmentName = "hoge"; VersionNo = -1 }
    try
      SQLite.update con department |> ignore
      fail ()
    with 
    | OptimisticLockException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex

  [<Test>]
  let ``updateIgnoreVersion``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let department = { DepartmentId = 1; DepartmentName = "hoge"; VersionNo = -1 }
    let department = SQLite.updateWithOpt con department (UpdateOpt(IgnoreVersion = true))
    printfn "%A" department

  [<Test>]
  let ``updateIgnoreVersion : no affected row``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let department = { DepartmentId = 0; DepartmentName = "hoge"; VersionNo = -1 }
    try
      SQLite.updateWithOpt con department (UpdateOpt(IgnoreVersion = true)) |> ignore
      fail ()
    with 
    | NoAffectedRowException _ as ex -> printfn "%s" (string ex)
    | ex -> fail ex
