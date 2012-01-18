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

namespace Soma.Core

open System
open System.Globalization
open System.Reflection
open System.Resources

type internal Message = 
  { Id : string;
    Text : string }
  member this.Format () = 
    "[" + this.Id + "] " + this.Text
  member this.AppendText (text:string) =
    { Id = this.Id; Text = this.Text + " " + text }

[<RequireQualifiedAccess>]
module internal SR =

  let private resources = 
    new ResourceManager("Resource", Assembly.GetExecutingAssembly())

  let private message id (args:obj array) =
    let resource = resources.GetString(id, CultureInfo.CurrentUICulture)
    let text = 
      if Array.isEmpty args then 
        resource 
      else
        String.Format(resource, args)
    { Id = id;
      Text = text }

  let private message0 (id, args:unit) = 
    message id Array.empty
  
  let private message1 (id, args:obj) = 
    message id [| args |]
  
  let private message2 (id, (arg1:obj, arg2:obj)) = 
    message id [| arg1; arg2 |]
   
  let private message3 (id, (arg1:obj, arg2:obj, arg3:obj)) = 
    message id [| arg1; arg2; arg3 |]

  let private message4 (id, (arg1:obj, arg2:obj, arg3:obj, arg4:obj)) = 
    message id [| arg1; arg2; arg3; arg4 |]

  let SOMA0001 args = message0 ("SOMA0001", args)
  let SOMA0002 args = message1 ("SOMA0002", args)

  (* expression *)
  let SOMA1000 args = message2 ("SOMA1000", args)
  let SOMA1001 args = message3 ("SOMA1001", args)
  let SOMA1002 args = message2 ("SOMA1002", args)
  let SOMA1003 args = message2 ("SOMA1003", args)
  let SOMA1004 args = message2 ("SOMA1004", args)
  let SOMA1005 args = message1 ("SOMA1005", args)
  let SOMA1006 args = message1 ("SOMA1006", args)
  let SOMA1007 args = message2 ("SOMA1007", args)
  let SOMA1009 args = message2 ("SOMA1009", args)
  let SOMA1011 args = message2 ("SOMA1011", args)
  let SOMA1012 args = message0 ("SOMA1012", args)
  let SOMA1013 args = message0 ("SOMA1013", args)
  let SOMA1014 args = message1 ("SOMA1014", args)
  let SOMA1015 args = message0 ("SOMA1015", args)
  let SOMA1016 args = message0 ("SOMA1016", args)
  let SOMA1017 args = message0 ("SOMA1017", args)
  let SOMA1018 args = message1 ("SOMA1018", args)
  let SOMA1019 args = message1 ("SOMA1019", args)
  let SOMA1020 args = message3 ("SOMA1020", args)
  let SOMA1021 args = message0 ("SOMA1021", args)
  let SOMA1022 args = message0 ("SOMA1022", args)
  let SOMA1023 args = message1 ("SOMA1023", args)
  let SOMA1024 args = message2 ("SOMA1024", args)
  let SOMA1025 args = message1 ("SOMA1025", args)
  let SOMA1026 args = message2 ("SOMA1026", args)
  let SOMA1027 args = message0 ("SOMA1027", args)

  (* sql *)
  let SOMA2000 args = message1 ("SOMA2000", args)
  let SOMA2001 args = message0 ("SOMA2001", args)
  let SOMA2002 args = message0 ("SOMA2002", args)
  let SOMA2003 args = message0 ("SOMA2003", args)
  let SOMA2004 args = message1 ("SOMA2004", args)
  let SOMA2005 args = message0 ("SOMA2005", args)
  let SOMA2006 args = message3 ("SOMA2006", args)
  let SOMA2007 args = message0 ("SOMA2007", args)
  let SOMA2008 args = message1 ("SOMA2008", args)
  let SOMA2009 args = message1 ("SOMA2009", args)
  let SOMA2010 args = message0 ("SOMA2010", args)
  let SOMA2011 args = message0 ("SOMA2011", args)
  let SOMA2012 args = message0 ("SOMA2012", args)
  let SOMA2013 args = message0 ("SOMA2013", args)
  let SOMA2014 args = message2 ("SOMA2014", args)
  let SOMA2015 args = message1 ("SOMA2015", args)
  let SOMA2016 args = message0 ("SOMA2016", args)
  let SOMA2017 args = message0 ("SOMA2017", args)
  let SOMA2018 args = message1 ("SOMA2018", args)
  let SOMA2019 args = message1 ("SOMA2019", args)

  (* meta *)
  let SOMA3000 args = message3 ("SOMA3000", args)
  let SOMA3001 args = message3 ("SOMA3001", args)
  let SOMA3002 args = message1 ("SOMA3002", args)
  let SOMA3003 args = message1 ("SOMA3003", args)
  let SOMA3004 args = message1 ("SOMA3004", args)
  let SOMA3005 args = message2 ("SOMA3005", args)
  let SOMA3006 args = message2 ("SOMA3006", args)
  let SOMA3007 args = message1 ("SOMA3007", args)
  let SOMA3008 args = message1 ("SOMA3008", args)
  let SOMA3009 args = message2 ("SOMA3009", args)

  (* db *)
  let SOMA4002 args = message0 ("SOMA4002", args)
  let SOMA4003 args = message2 ("SOMA4003", args)
  let SOMA4004 args = message0 ("SOMA4004", args)
  let SOMA4005 args = message1 ("SOMA4005", args)
  let SOMA4007 args = message0 ("SOMA4007", args)
  let SOMA4010 args = message0 ("SOMA4010", args)
  let SOMA4011 args = message2 ("SOMA4011", args)
  let SOMA4012 args = message3 ("SOMA4012", args)
  let SOMA4013 args = message2 ("SOMA4013", args)
  let SOMA4014 args = message3 ("SOMA4014", args)
  let SOMA4015 args = message2 ("SOMA4015", args)
  let SOMA4016 args = message2 ("SOMA4016", args)
  let SOMA4017 args = message4 ("SOMA4017", args)
  let SOMA4018 args = message4 ("SOMA4018", args)
  let SOMA4019 args = message2 ("SOMA4019", args)
  let SOMA4020 args = message0 ("SOMA4020", args)
  let SOMA4021 args = message0 ("SOMA4021", args)
  let SOMA4022 args = message3 ("SOMA4022", args)
  let SOMA4023 args = message4 ("SOMA4023", args)
  let SOMA4024 args = message0 ("SOMA4024", args)
  let SOMA4025 args = message0 ("SOMA4025", args)
  let SOMA4027 args = message3 ("SOMA4027", args)

  (* quotation *)
  let SOMA5000 args = message1 ("SOMA5000", args)