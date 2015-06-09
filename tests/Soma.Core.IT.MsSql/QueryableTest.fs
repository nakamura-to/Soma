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

module QueryableTest = 

    type CommandVariable = {
        Name : string
        DataType : SqlDbType
        Value : obj
    }
    
    let assert_one_query (f : unit -> unit) expectedCommandText (expectedParameters : CommandVariable seq) =
        let beforeQueryCount = MsSql.queriesExecuted |> Seq.length
        f()
        let afterQueryCount = MsSql.queriesExecuted |> Seq.length
        if beforeQueryCount = afterQueryCount then
            Assert.Fail("No Queries were executed.")
        if (beforeQueryCount + 1) > afterQueryCount then
            Assert.Fail("More than one query was executed.")
        let lastCommand, state = MsSql.queriesExecuted |> Seq.last
        Assert.AreEqual(expectedCommandText, lastCommand.CommandText)

        let actualParameters = lastCommand.Parameters |> Seq.cast<SqlClient.SqlParameter> |> Seq.map(fun param ->
            {
                Name = param.ParameterName
                DataType = param.SqlDbType
                Value = param.Value
            }
        )
        
        areSeqEqual expectedParameters actualParameters

    [<Test>]
    let ``find``() =

        assert_one_query (fun () -> 
            let department = query {
                for d in MsSql.queryable<Department>() do
                where(d.DepartmentId = 2)
                exactlyOne
            }
            printfn "%A" department
            assert_equal { DepartmentId = 2; DepartmentName = "Sales"; VersionNo = 0; } department
        ) "SELECT TOP 2 T.DepartmentId, T.DepartmentName, T.VersionNo FROM Department AS T WHERE (T.DepartmentId = @p1)" [
            {Name="@p1"; DataType = SqlDbType.Int; Value = 2}
        ]

    [<Test>]
    let ``find : by composite id``() =
        assert_one_query (fun () -> 
            let employee = query {
                for e in MsSql.queryable<CompKeyEmployee>() do
                where(e.EmployeeId1 = 2)
                where(e.EmployeeId2 = 12)
                exactlyOne
            }
            assert_equal { EmployeeId1 = 2; EmployeeId2 = 12; EmployeeName = "Smith"; VersionNo = 0; } employee
        ) "SELECT TOP 2 T.EmployeeId1, T.EmployeeId2, T.EmployeeName, T.VersionNo FROM CompKeyEmployee AS T WHERE (T.EmployeeId1 = @p1 AND T.EmployeeId2 = @p2)" [
            {Name="@p1"; DataType = SqlDbType.Int; Value = 2}
            {Name="@p2"; DataType = SqlDbType.Int; Value = 12}
        ]

    [<Test>]
    let ``find : id type is option``() =
        assert_one_query (fun () -> 
            let employee = query {
                for e in MsSql.queryable<Employee>() do
                where(e.EmployeeId = Some 1)
                exactlyOne
            }
            assert_equal { EmployeeId = Some 1; DepartmentId = Some 1; EmployeeName = Some "King"; VersionNo = Some 0; } employee
        ) "SELECT TOP 2 T.EmployeeId, T.EmployeeName, T.DepartmentId, T.VersionNo FROM Employee AS T WHERE (T.EmployeeId = @p1)" [
            {Name="@p1"; DataType = SqlDbType.Int; Value = 1}
        ]

    [<Test>]
    let ``find : not found``() =
        try
            let employee = query {
                for d in MsSql.queryable<Department>() do
                where(d.DepartmentId = 99)
                exactlyOne
            }
            fail ()
        with 
        | :? System.InvalidOperationException as ex when(ex.Message = "Sequence contains no elements") -> ignore()
        | ex -> fail ex

    [<Test>]
    let ``column names resolve``() =
        assert_one_query (fun () -> 
            let person = query {
                for e in MsSql.queryable<PersonDifferent>() do
                where(e.PersonNameOf = "Martin")
                exactlyOne
            }
            assert_equal { PersonIdentifier = 2; PersonNameOf = "Martin"; JobKind = JobKind.Manager; VersionNo = 0; } person
        ) "SELECT TOP 2 T.PersonId, T.PersonName, T.JobKind, T.VersionNo FROM Person AS T WHERE (T.PersonName = @p1)" [
            {Name="@p1"; DataType = SqlDbType.NVarChar; Value = "Martin"}
        ]
    [<Test>]
    let ``direct sql``() =
        assert_one_query (fun () -> 
            let persons = 
                MsSql.queryableDirectSql<PersonDifferent>
                    [Soma.Core.Sql.S "SELECT TOP 2 T.PersonId, T.PersonName, T.JobKind, T.VersionNo FROM Person AS T WHERE T.PersonName = "; Soma.Core.Sql.P "Martin"]
                    []
            let person = Seq.exactlyOne persons
            assert_equal { PersonIdentifier = 2; PersonNameOf = "Martin"; JobKind = JobKind.Manager; VersionNo = 0; } person
        ) "SELECT TOP 2 T.PersonId, T.PersonName, T.JobKind, T.VersionNo FROM Person AS T WHERE T.PersonName = @p1" [
            {Name="@p1"; DataType = SqlDbType.NVarChar; Value = "Martin"}
        ]

//    add test for type with differnt table name, and columns with different table names.
//    use PersonDifferent
//    [<Test>]
//    let ``tryFindWithVersion : optimistic lock confliction``() =
//        try
//            MsSql.tryFindWithVersion<Department> [2] 99 |> ignore
//            fail ()
//        with
//        | OptimisticLockException _ as ex -> printfn "%A" ex
