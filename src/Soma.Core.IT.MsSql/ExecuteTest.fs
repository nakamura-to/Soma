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

module ExecuteTest = 

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
  let ``execute : insert``() =
    use ts = new TransactionScope()
    let rows = 
      MsSql.execute "
        insert into Department (DepartmentId, DepartmentName, VersionNo) values (/* id */0, /* name */'aaa', /* version */0)
        " ["id" @= 99; "name" @= "hoge"; "version" @= 10] 
    assert_equal 1 rows

  [<Test>]
  let ``execute : update``() =
    use ts = new TransactionScope()
    let rows = 
      MsSql.execute "
        update Department set DepartmentName = N'hoge' where DepartmentId = /* id */-1 and VersionNo = /* versionNo */-1
        " ["id" @= 1; "versionNo" @= 0]
    assert_equal 1 rows

  [<Test>]
  let ``execute : delete``() =
    use ts = new TransactionScope()
    let rows = 
      MsSql.execute "
        delete from Department
        " []
    assert_equal 2 rows

  [<Test>]
  let ``executeReader``() =
    use ts = new TransactionScope()
    let table = 
      MsSql.executeReader (fun reader -> 
        let table = new DataTable()
        table.Load reader
        table ) "
        select * from Department where DepartmentId = /* id */0
        " ["id" @= 2]
    assert_equal 1 table.Rows.Count
    let row = table.Rows.[0]
    assert_equal "Sales" row.["DepartmentName"]
