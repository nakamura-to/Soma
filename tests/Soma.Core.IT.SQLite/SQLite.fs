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
open System.Data.Common
open Soma.Core

module SQLite = 

  let config = 
    { new SQLiteConfig() with
      member this.ConnectionString = "Data Source=..\..\Soma.Core.IT.db" }
  let query<'T> = LocalDb.query<'T> config
  let queryOnDemand<'T> = LocalDb.queryOnDemand<'T> config
  let paginate<'T> = LocalDb.paginate<'T> config
  let paginateOnDemand<'T>  = LocalDb.paginateOnDemand<'T> config
  let paginateAndCount<'T> = LocalDb.paginateAndCount<'T> config
  let execute sql expr = LocalDb.execute config sql expr
  let find<'T when 'T : not struct> = LocalDb.find<'T> config
  let tryFind<'T when 'T : not struct> = LocalDb.tryFind<'T> config
  let findWithVersion<'T when 'T : not struct> = LocalDb.findWithVersion<'T> config
  let tryFindWithVersion<'T when 'T : not struct> = LocalDb.tryFindWithVersion<'T> config
  let insert<'T when 'T : not struct> = LocalDb.insert<'T> config
  let update<'T when 'T : not struct> = LocalDb.update<'T> config
  let updateWithOpt<'T when 'T : not struct> = LocalDb.updateWithOpt<'T> config
  let delete<'T when 'T : not struct> = LocalDb.delete<'T> config
  let deleteWithOpt<'T when 'T : not struct> = LocalDb.deleteWithOpt<'T> config
  let call<'T when 'T : not struct> = LocalDb.call<'T> config
  let createConnection () = LocalDb.createConnection config
