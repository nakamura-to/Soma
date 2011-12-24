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

module LocalDbClassTest = 

  type Department() =
    let mutable departmentId:int = 0
    let mutable departmentName:string = null
    let mutable versionNo:int = 0
    [<Id>]
    member this.DepartmentId 
      with get () = departmentId
      and  set (v) = departmentId <- v
    member this.DepartmentName 
      with get () = departmentName
      and  set (v) = departmentName <- v
    [<Version>]
    member this.VersionNo
      with get () = versionNo
      and  set (v) = versionNo <- v

  [<AllowNullLiteral>]
  type Employee() =
    let mutable employeeId:int Nullable = Nullable ()
    let mutable employeeName:string  = null
    let mutable departmentId:int Nullable = Nullable ()
    let mutable versionNo:int Nullable = Nullable ()
    [<Id(IdKind.Identity)>]
    member this.EmployeeId 
      with get () = employeeId
      and  set (v) = employeeId <- v
    member this.EmployeeName 
      with get () = employeeName
      and  set (v) = employeeName <- v
    member this.DepartmentId 
      with get () = departmentId
      and  set (v) = departmentId <- v
    [<Version>]
    member this.VersionNo
      with get () = versionNo
      and  set (v) = versionNo <- v

  let db = LocalDb(SQLite.config)

  [<Test>]
  let ``insert``() =
    use tx = new TransactionScope ()
    use con = db.CreateConnection()
    let employee = Employee(EmployeeName = "Hoge")
    db.Insert(con, employee)
    assert_true employee.EmployeeId.HasValue

  [<Test>]
  let ``update``() =
    use tx = new TransactionScope ()
    use con = db.CreateConnection()
    let employee = db.Find<Employee>(con, 1)
    employee.EmployeeName <- "Hoge"
    db.Update(con, employee)
    let employee = db.Find<Employee>(con, 1)
    assert_equal "Hoge" employee.EmployeeName

  [<Test>]
  let ``delete``() =
    use tx = new TransactionScope ()
    use con = db.CreateConnection()
    let employee = db.Find<Employee>(con, 1)
    db.Delete(con, employee)
    let employee = db.TryFind<Employee>(con, 1)
    assert_equal null employee
