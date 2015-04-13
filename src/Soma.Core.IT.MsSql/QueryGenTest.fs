namespace Soma.Core.IT

open System
open System.Configuration
open System.Data
open System.Data.Common
open System.Transactions
open NUnit.Framework
open Soma.Core

module QueryGenTest = 

  [<Test>]
  let ``simple select``() =
    ignore()
//    let allPersons = query {
//        for p in MsSql.queryable<Person>() do
//        select p
//    }
    
    
    //translate allPersons.Expression