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
open Soma.Core

module MsSql = 
    let connectionObserver = 
        { new IConnectionObserver with
              member this.NotifyOpening(connection, [<System.Runtime.InteropServices.Out>] userState) = ignore()
              member this.NotifyOpened(connection, userState) = ignore() }
    
    let mutable queriesExecuted = List.empty<Data.IDbCommand * obj>
    
    let commandObserver = 
        { new ICommandObserver with
              member this.NotifyExecuting(command, [<System.Runtime.InteropServices.Out>] userState : byref<obj>) = 
                  ignore()
              member this.NotifyExecuted(command, userState : obj) = 
                  queriesExecuted <- queriesExecuted @ [ command, userState ] }
    
    let connectionString = 
        let path = "..\..\connection.txt"
        if IO.File.Exists(path) then IO.File.ReadAllText path
        else "Data Source=.\SQLEXPRESS;Initial Catalog=Soma.Core.IT;Integrated Security=True"
    
    let config = 
        { new MsSqlConfig() with
              member this.ConnectionString = connectionString
              member this.ConnectionObserver = connectionObserver
              member this.CommandObserver = commandObserver }
    
    let query<'T> = Db.query<'T> config
    let queryOnDemand<'T> = Db.queryOnDemand<'T> config
    let paginate<'T> = Db.paginate<'T> config
    let paginateOnDemand<'T> = Db.paginateOnDemand<'T> config
    let paginateAndCount<'T> = Db.paginateAndCount<'T> config
    let execute sql expr = Db.execute config sql expr
    let executeReader<'T> handler sql expr = Db.executeReader<'T> config handler sql expr
    let find<'T when 'T : not struct> = Db.find<'T> config
    let tryFind<'T when 'T : not struct> = Db.tryFind<'T> config
    let findWithVersion<'T when 'T : not struct> = Db.findWithVersion<'T> config
    let tryFindWithVersion<'T when 'T : not struct> = Db.tryFindWithVersion<'T> config
    let insert<'T when 'T : not struct> = Db.insert<'T> config
    let insertWithOpt<'T when 'T : not struct> = Db.insertWithOpt<'T> config
    let update<'T when 'T : not struct> = Db.update<'T> config
    let updateWithOpt<'T when 'T : not struct> = Db.updateWithOpt<'T> config
    let insertOrUpdate<'T when 'T : not struct> = Db.insertOrUpdate<'T> config
    let delete<'T when 'T : not struct> = Db.delete<'T> config
    let deleteWithOpt<'T when 'T : not struct> = Db.deleteWithOpt<'T> config
    let call<'T when 'T : not struct> = Db.call<'T> config
    let queryable<'T when 'T : not struct>() = Db.queryable<'T> config
    let queryableDirectSql<'T when 'T : not struct> = Db.queryableDirectSql<'T> config
    let queryableDelete<'T when 'T : not struct> = Db.queryableDelete<'T> config
