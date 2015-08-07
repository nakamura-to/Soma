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
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Soma.Core.ExpressionAst

type internal ExpressionException =
  inherit InvalidOperationException
  new : message:Message * ?innerException:exn -> ExpressionException
  member MessageId : string

/// <summary>Provides the expression operations.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Expression = 

  /// <summary>Parses the expression.</summary>
  /// <param name="expr">The expression.</param>
  /// <returns>The parsed expression AST.</returns>
  [<CompiledName("Parse")>]
  val parse : expr:string -> Expression

  val internal evaluate : string -> IDictionary<string, obj * Type> -> Func<string, Expression> -> obj * Type
