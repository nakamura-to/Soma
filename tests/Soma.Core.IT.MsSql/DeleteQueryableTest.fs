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

module DeleteQueryableTest = 

    [<Test>]
    let ``delete : no id``() = 
        use ts = new TransactionScope()
        let getInDB () = 
            query { 
                for n in MsSql.queryable<NoId>() do
                exists(n.Name = "aaa" && n.VersionNo = 0)
            }

        if not (getInDB()) then
            MsSql.insert { NoId.Name = "aaa"
                           VersionNo = 0 } |> ignore

        Assert.IsTrue(getInDB())

        MsSql.queryableDelete(query { 
            for n in MsSql.queryable<NoId>() do
            where(n.Name = "aaa" && n.VersionNo = 0)
        })

        Assert.IsFalse(getInDB())
    
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

        MsSql.queryableDelete(query { 
            for n in MsSql.queryable<NoVersion>() do
            where(n.Id = 1)
        })

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

        MsSql.queryableDelete(query { 
            for d in MsSql.queryable<Department>() do
            where(d.DepartmentId = 1)
        })

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

        MsSql.queryableDelete(query { 
            for a in MsSql.queryable<Address>() do
            where(a.AddressId = inserted.AddressId)
        })
    
        Assert.IsFalse(getInDB())