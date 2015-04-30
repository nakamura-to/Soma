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

module PaginateTest = 

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

  type Address =
    { [<Id(IdKind.Identity)>]
      AddressId : int
      Street : string
      [<Version>]
      VersionNo : byte array }

  [<Test>]
  let ``paginate``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginate<Employee> con "
        select * from Employee order by EmployeeId
        " [] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : offset is positive``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginate<Employee> con "
        select * from Employee order by EmployeeId
        " [] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : offset is zero``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginate<Employee> con "
        select * from Employee order by EmployeeId
        " [] (0L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : offset is negative``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginate<Employee> con "
        select * from Employee order by EmployeeId
        " [] (-1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : limit is positive``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginate<Employee> con "
        select * from Employee order by EmployeeId
        " [] (1L, 1L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 1 employees.Length

  [<Test>]
  let ``paginate : limit is zero``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginate<Employee> con "
        select * from Employee order by EmployeeId
        " [] (1L, 0L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 0 employees.Length

  [<Test>]
  let ``paginate : limit is negative``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginate<Employee> con "
        select * from Employee order by EmployeeId
        " [] (1L, -1L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 3 employees.Length

  [<Test>]
  let ``paginateOnDemand``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginateOnDemand<Employee> con "
        select * from Employee order by EmployeeId
        " [] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    let employees = Seq.toList employees
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginateOnDemand : offset is zero``() =
    use con = MsSqlCe.createConnection()
    let employees = 
      MsSqlCe.paginateOnDemand<Employee> con "
        select * from Employee order by EmployeeId
        " [] (0L, 2L)
    employees |> Seq.iter (printfn "%A")
    let employees = Seq.toList employees
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginateAndCount``() =
    use con = MsSqlCe.createConnection()
    let employees, count = 
      MsSqlCe.paginateAndCount<Employee> con "
        select * from Employee order by EmployeeId
        " [] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length
    assert_equal 4L count