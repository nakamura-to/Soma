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

  [<Test>]
  let ``execute : insert``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let rows = 
      SQLite.execute con "
        insert into Department (DepartmentId, DepartmentName, VersionNo) values (/* id */0, /* name */'aaa', /* version */0)
        " ["id" @= 99; "name" @= "hoge"; "version" @= 10]
    assert_equal 1 rows

  [<Test>]
  let ``execute : update``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let rows = 
      SQLite.execute con "
        update Department set DepartmentName = 'hoge' where DepartmentId = /* id */-1 and VersionNo = /* versionNo */-1
        " ["id" @= 1; "versionNo" @= 0] 
    assert_equal 1 rows

  [<Test>]
  let ``execute : delete``() =
    use ts = new TransactionScope()
    use con = SQLite.createConnection()
    let rows = 
      SQLite.execute con "
        delete from Department
        " [] 
    assert_equal 2 rows