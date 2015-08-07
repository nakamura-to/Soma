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

module DeleteTest = 
    [<Test>]
    let ``delete : no id``() = 
        use ts = new TransactionScope()
        let ex = Assert.Throws<DbException>(fun() -> 
            MsSql.delete { NoId.Name = "aaa"
                           VersionNo = 0 }
        ) 
        Assert.IsTrue(ex.Message.StartsWith "[SOMA4005]")
    
    [<Test>]
    let ``delete : no version``() = 
        use ts = new TransactionScope()
        let getInDB () = 
            query { 
                for n in MsSql.queryable<NoVersion>() do
                exists(n.Id = 1)
            }

        if not (getInDB()) then
            MsSql.insert { NoVersion.Id = 1
                           Name = "aaa" } |> ignore
        Assert.IsTrue(getInDB())

        MsSql.delete { NoVersion.Id = 1
                       Name = "aaa" }

        Assert.IsFalse(getInDB())

    [<Test>]
    let ``delete : incremented version``() = 
        use ts = new TransactionScope()
        let getInDB () = 
            query { 
                for d in MsSql.queryable<Department>() do
                exists(d.DepartmentId = 1)
            }

        if not (getInDB()) then
            MsSql.insert { DepartmentId = 1
                           DepartmentName = "aaa"
                           VersionNo = 0 } |> ignore

        Assert.IsTrue(getInDB())

        let department = MsSql.find<Department> [ 1 ]
        MsSql.delete department

        Assert.IsFalse(getInDB())
    
    [<Test>]
    let ``delete : computed version``() = 
        use ts = new TransactionScope()

        let inserted = 
            MsSql.insert { AddressId = 0
                           Street = "hoge"
                           VersionNo = Array.empty }

        let getInDB () = 
            query { 
                for a in MsSql.queryable<Address>() do
                exists(a.AddressId = inserted.AddressId)
            }

        Assert.IsTrue(getInDB())

        MsSql.delete inserted
    
        Assert.IsFalse(getInDB())

    [<Test>]
    let ``delete : incremented version : optimistic lock confliction``() = 
        use ts = new TransactionScope()
        
        let department = 
            { DepartmentId = 1
              DepartmentName = "hoge"
              VersionNo = -1 }
        let ex = Assert.Throws<OptimisticLockException>(fun() -> 
            MsSql.delete department 
        ) 
        ignore() 
    
    [<Test>]
    let ``delete : computed version : optimistic lock confliction``() = 
        use ts = new TransactionScope()
        
        let address = 
            { AddressId = 1
              Street = "hoge"
              VersionNo = Array.empty }
        let ex = Assert.Throws<OptimisticLockException>(fun() -> 
            MsSql.delete address
        ) 
        ignore() 
    
    [<Test>]
    let deleteIgnoreVersion() = 
        use ts = new TransactionScope()
        
        let getInDB () = 
            query { 
                for d in MsSql.queryable<Department>() do
                exists(d.DepartmentId = 1)
            }

        if not (getInDB()) then
            MsSql.insert { DepartmentId = 1
                           DepartmentName = "aaa"
                           VersionNo = -1 } |> ignore

        Assert.IsTrue(getInDB())

        let department = 
            { DepartmentId = 1
              DepartmentName = "aaa"
              VersionNo = -1 }
        MsSql.deleteWithOpt department (DeleteOpt(IgnoreVersion = true))
    
        Assert.IsFalse(getInDB())

    [<Test>]
    let ``deleteIgnoreVersion : no affected row``() = 
        use ts = new TransactionScope()
        
        let department = 
            { DepartmentId = 0
              DepartmentName = "aaa"
              VersionNo = -1 }
        let ex = Assert.Throws<NoAffectedRowException>(fun() -> 
            MsSql.deleteWithOpt department (DeleteOpt(IgnoreVersion = true))
        ) 
        ignore()