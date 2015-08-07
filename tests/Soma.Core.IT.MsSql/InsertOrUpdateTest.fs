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

module InsertOrUpdateTest = 
    [<Test>]
    let ``update : no id``() = 
        use ts = new TransactionScope()
        
        let ex = 
            Assert.Throws<NoIdPropertiesException>(fun () -> 
                MsSql.insertOrUpdate { NoId.Name = "aaa"
                                       VersionNo = 0 }
                |> ignore)
        Assert.True(ex.Message.StartsWith "[SOMA4005]")
    
    [<Test>]
    let ``update : composite id``() = 
        use ts = new TransactionScope()
        let employee = MsSql.find<CompKeyEmployee> [ 2; 12 ]
        let employee = MsSql.insertOrUpdate { employee with EmployeeName = "hoge" }
        printfn "%A" employee
        assert_equal { EmployeeId1 = 2
                       EmployeeId2 = 12
                       EmployeeName = "hoge"
                       VersionNo = 1 } employee
    
    [<Test>]
    let ``update : no version``() = 
        use ts = new TransactionScope()
        
        let noVersion = 
            MsSql.insertOrUpdate { NoVersion.Id = 1
                                   Name = "aaa" }
        printfn "%A" noVersion
        assert_equal { NoVersion.Id = 1
                       Name = "aaa" } noVersion
    
    [<Test>]
    let ``update : incremented version``() = 
        use ts = new TransactionScope()
        
        let department = 
            { DepartmentId = 1
              DepartmentName = "hoge"
              VersionNo = 0 }
        
        let department = MsSql.insertOrUpdate department
        printfn "%A" department
        assert_equal 1 department.VersionNo
    
    [<Test>]
    let ``update : computed version``() = 
        use ts = new TransactionScope()
        
        let address = 
            MsSql.insertOrUpdate { Address.AddressId = 0
                                   Street = "hoge"
                                   VersionNo = Array.empty }
        
        let address = { address with Street = "foo" }
        let version = address.VersionNo
        let address = MsSql.insertOrUpdate address
        printfn "%A" address
        Assert.True(version <> address.VersionNo)
    
    [<Test>]
    let ``update : Enum``() = 
        use ts = new TransactionScope()
        let loaded = MsSql.find<Person> [ 2 ]
        let updated = MsSql.insertOrUpdate { loaded with JobKind = JobKind.Salesman }
        printfn "%A" updated
        assert_equal { PersonId = 2
                       PersonName = "Martin"
                       JobKind = JobKind.Salesman
                       VersionNo = 1 } updated
    
    [<Test; Ignore("This does not work sql server is not throwing the exception.")>]
    let ``update : unique constraint violation``() = 
        use ts = new TransactionScope()
        
        let department = 
            { DepartmentId = 1
              DepartmentName = "Sales"
              VersionNo = 0 }
        Assert.Throws<UniqueConstraintException>(fun () -> MsSql.insertOrUpdate department |> ignore) |> ignore
    
    //optimistic locking is not supported with upserts.
    //  [<Test>]
    //  let ``update : optimistic lock confliction``() =
    //    use ts = new TransactionScope()
    //    let department = { DepartmentId = 1; DepartmentName = "hoge"; VersionNo = -1 }
    //    Assert.Throws<OptimisticLockException>(fun () -> 
    //        MsSql.insertOrUpdate department |> ignore
    //    ) |> ignore
    //  [<Test>]
    //  let ``updateIgnoreVersion``() =
    //    use ts = new TransactionScope()
    //    let department = { DepartmentId = 1; DepartmentName = "hoge"; VersionNo = -1 }
    //    let department = MsSql.insertOrUpdateWithOpt department (UpdateOpt(IgnoreVersion = true))
    //    printfn "%A" department
    //
    //  [<Test>]
    //  let ``updateIgnoreVersion : no affected row``() =
    //    use ts = new TransactionScope()
    //    let department = { DepartmentId = 0; DepartmentName = "hoge"; VersionNo = -1 }
    //    try
    //      MsSql.insertOrUpdateWithOpt department (UpdateOpt(IgnoreVersion = true)) |> ignore
    //      fail()
    //    with 
    //    | NoAffectedRowException _ as ex -> printfn "%s" (string ex)
    //    | ex -> fail ex
    //
    //  [<Test>]
    //  let ``update : exclude null``() =
    //    use ts = new TransactionScope()
    //    let employee = MsSql.find<Employee> [1]
    //    let employee = MsSql.insertOrUpdateWithOpt { employee with EmployeeName = None} (UpdateOpt(ExcludeNull = true))
    //    assert_equal { EmployeeId = Some 1; EmployeeName = None; DepartmentId = Some 1; VersionNo = Some 1; } employee
    //
    //  [<Test>]
    //  let ``update : exclude``() =
    //    use ts = new TransactionScope()
    //    let employee = MsSql.find<Employee> [1]
    //    let employee = MsSql.insertOrUpdateWithOpt { employee with EmployeeName = Some "hoge"; DepartmentId = Some 2} (UpdateOpt(Exclude = ["EmployeeName"]))
    //    let employee = MsSql.find<Employee> [1]
    //    assert_equal { EmployeeId = Some 1; EmployeeName = Some "King"; DepartmentId = Some 2; VersionNo = Some 1; } employee
    //
    //  [<Test>]
    //  let ``update : include``() =
    //    use ts = new TransactionScope()
    //    let employee = MsSql.find<Employee> [1]
    //    let employee = MsSql.insertOrUpdateWithOpt { employee with EmployeeName = Some "hoge"; DepartmentId = Some 2} (UpdateOpt(Include = ["EmployeeName"]))
    //    let employee = MsSql.find<Employee> [1]
    //    assert_equal { EmployeeId = Some 1; EmployeeName = Some "hoge"; DepartmentId = Some 1; VersionNo = Some 1; } employee
    [<Test>]
    let ``insert : assigned id``() = 
        use ts = new TransactionScope()
        
        let department = 
            { DepartmentId = 99
              DepartmentName = "aaa"
              VersionNo = 0 }
        
        let department = MsSql.insertOrUpdate department
        printfn "%A" department
        assert_equal 99 department.DepartmentId
        assert_equal 1 department.VersionNo
    
    [<Test>]
    let ``insert : assigned composite id``() = 
        use ts = new TransactionScope()
        
        let employee = 
            { EmployeeId1 = 99
              EmployeeId2 = 1
              EmployeeName = "aaa"
              VersionNo = 0 }
        
        let employee = MsSql.insertOrUpdate employee
        printfn "%A" employee
        assert_equal 99 employee.EmployeeId1
        assert_equal 1 employee.EmployeeId2
        assert_equal 1 employee.VersionNo
    
    [<Test>]
    let ``insert : identity id``() = 
        use ts = new TransactionScope()
        
        let employee = 
            { Employee.EmployeeId = None
              EmployeeName = Some "hoge"
              DepartmentId = Some 1
              VersionNo = Some 0 }
        
        let employee = MsSql.insertOrUpdate employee
        printfn "%A" employee
        assert_true employee.EmployeeId.IsSome
        assert_equal 1 employee.VersionNo.Value
    
    [<Test>]
    let ``insert : no id``() = 
        use ts = new TransactionScope()
        
        let noId = 
            { NoId.Name = "aaa"
              VersionNo = 0 }
        try 
            MsSql.insertOrUpdate noId |> ignore
            fail()
        with
        | :? NoIdPropertiesException as ex -> 
            Assert.True(ex.Message.StartsWith "[SOMA4005]")
            printfn "%A" ex
        | ex -> fail ex
    
    [<Test>]
    let ``insert : no version``() = 
        use ts = new TransactionScope()
        
        let noVersion = 
            { NoVersion.Id = 99
              Name = "aaa" }
        
        let noVersion = MsSql.insertOrUpdate noVersion
        printfn "%A" noVersion
        assert_equal { NoVersion.Id = 99
                       Name = "aaa" } noVersion
    
    // this wont fail, it does an update instead of an insert
    //  [<Test>]
    //  let ``insert : unique constraint violation : unique key``() =
    //    use ts = new TransactionScope()
    //    let department = { DepartmentId = 1; DepartmentName = "aaa"; VersionNo = 0 }
    //    Assert.Throws<UniqueConstraintException>(fun () -> MsSql.insertOrUpdate department |> ignore) |> ignore
    [<Test; Ignore("This does not work sql server is not throwing the exception.")>]
    let ``insert : unique constraint violation : unique index``() = 
        use ts = new TransactionScope()
        
        let person = 
            { PersonId = 0
              PersonName = "Scott"
              JobKind = JobKind.Salesman
              VersionNo = 0 }
        try 
            MsSql.insertOrUpdate person |> ignore
            fail()
        with
        | UniqueConstraintException _ as ex -> printfn "%s" (string ex)
        | ex -> fail ex
    
    [<Test>]
    let ``insert : incremented version``() = 
        use ts = new TransactionScope()
        
        let employee = 
            { Employee.EmployeeId = None
              EmployeeName = None
              DepartmentId = Some 1
              VersionNo = None }
        
        let employee = MsSql.insertOrUpdate employee
        printfn "%A" employee
        assert_true employee.VersionNo.IsSome
        assert_equal 1 employee.VersionNo.Value
    
    [<Test>]
    let ``insert : computed version``() = 
        use ts = new TransactionScope()
        
        let address = 
            { Address.AddressId = 0
              Street = "hoge"
              VersionNo = Array.empty }
        
        let address = MsSql.insertOrUpdate address
        printfn "%A" address
        assert_true <| not (Array.isEmpty address.VersionNo)
//  [<Test>]
//  let ``insert : exclude null``() =
//    use ts = new TransactionScope()
//    let employee = { Employee.EmployeeId = None; EmployeeName = None; DepartmentId = None; VersionNo = Some 0 }
//    MsSql.insertOrUpdateWithOpt employee (InsertOpt(ExcludeNull = true)) |> ignore
//
//  [<Test>]
//  let ``insert : exclude``() =
//    use ts = new TransactionScope()
//    let employee = { Employee.EmployeeId = None; EmployeeName = Some "hoge"; DepartmentId = Some 1; VersionNo = Some 0 }
//    let employee = MsSql.insertOrUpdateWithOpt employee (InsertOpt(Exclude = ["EmployeeName"]))
//    let employee = MsSql.find<Employee> [employee.EmployeeId]
//    assert_true employee.EmployeeName.IsNone
//    assert_equal (Some 1) employee.DepartmentId
//
//  [<Test>]
//  let ``insert : include``() =
//    use ts = new TransactionScope()
//    let employee = { Employee.EmployeeId = None; EmployeeName = Some "hoge"; DepartmentId = Some 1; VersionNo = Some 0 }
//    let employee = MsSql.insertOrUpdateWithOpt employee (InsertOpt(Include = ["EmployeeName"]))
//    let employee = MsSql.find<Employee> [employee.EmployeeId]
//    assert_true employee.EmployeeId.IsSome
//    assert_equal None employee.DepartmentId
