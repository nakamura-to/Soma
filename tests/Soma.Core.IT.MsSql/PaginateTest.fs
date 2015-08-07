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

module PaginateTest = 

  [<Test>]
  let ``paginate``() =
    let employees = 
      MsSql.paginate<Employee> "
        select * from Employee order by EmployeeId
        " [] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : offset is positive``() =
    let employees = 
      MsSql.paginate<Employee> "
        select * from Employee order by EmployeeId
        " [] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : offset is positive : embedded variable in order by clause``() =
    let employees = 
      MsSql.paginate<Employee> "
        select * from Employee E order by /*#orderby*/
        " ["orderby" @= "E.EmployeeId"] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : offset is zero``() =
    let employees = 
      MsSql.paginate<Employee> "
        select * from Employee order by EmployeeId
        " [] (0L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : offset is negative``() =
    let employees = 
      MsSql.paginate<Employee> "
        select * from Employee order by EmployeeId
        " [] (-1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginate : limit is positive``() =
    let employees = 
      MsSql.paginate<Employee> "
        select * from Employee order by EmployeeId
        " [] (1L, 1L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 1 employees.Length

  [<Test>]
  let ``paginate : limit is zero``() =
    let employees = 
      MsSql.paginate<Employee> "
        select * from Employee order by EmployeeId
        " [] (1L, 0L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 0 employees.Length

  [<Test>]
  let ``paginate : limit is negative``() =
    let employees = 
      MsSql.paginate<Employee> "
        select * from Employee order by EmployeeId
        " [] (1L, -1L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 3 employees.Length

  [<Test>]
  let ``paginateOnDemand``() =
    let employees = 
      MsSql.paginateOnDemand<Employee> "
        select * from Employee order by EmployeeId
        " [] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    let employees = Seq.toList employees
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginateOnDemand : offset is zero``() =
    let employees = 
      MsSql.paginateOnDemand<Employee> "
        select * from Employee order by EmployeeId
        " [] (0L, 2L)
    employees |> Seq.iter (printfn "%A")
    let employees = Seq.toList employees
    assert_equal 2 employees.Length

  [<Test>]
  let ``paginateAndCount``() =
    let employees, count = 
      MsSql.paginateAndCount<Employee> "
        select * from Employee order by EmployeeId
        " [] (1L, 2L)
    employees |> Seq.iter (printfn "%A")
    assert_equal 2 employees.Length
    assert_equal 4L count