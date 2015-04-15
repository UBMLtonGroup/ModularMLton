(* Copyright (C) 2015 Nathan G. Burgers.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor HookGC (S : SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

open S

(*
 * HookGC identifies all operations involving heap object
 * creation and access, and converts them into CCall's
 * into the modular garbage collector.
 *)

(* 
 * TODO(nate): modify the `Type` structure to interface with
 * the modular GC.
 *)

open CFunction

fun arrayAllocateHook type_parameter =
  CFunction.T { args = Vector.new0 ()
	      , convention = Convention.Cdecl
	      , kind = Kind.Impure
	      , prototype = (Vector.new0 (), SOME CType.objptr)
	      , return = Type.array type_parameter
	      , symbolScope = SymbolScope.External
	      , target = Target.Direct "gc_allocate_array" }

fun hookPrimitiveApplication ( arguments, primitive, type_parameters ) =
    let val primitive = 
	    case Prim.name primitive of
		Prim.Name.Array_array => { args = Vector.new0 ()
					 , prim = Prim.ffi (arrayAllocateHook (Vector.sub (type_parameters, 0)))
					 , targs = Vector.new0 () }
	      (* TODO(nate): non-exhaustive list of primitives to implement *)
	      (* | Prim.Array_length =>  *)
	      (* | Prim.Array_sub =>  *)
	      (* | Prim.Array_toVector =>  *)
	      (* | Prim.Array_update =>  *)
	      (* | Prim.GC_collect =>  *)
	      (* | Prim.Vector_length =>  *)
	      (* | Prim.Vector_sub =>  *)
	      (* | Prim.Weak_canGet =>  *)
	      (* | Prim.Weak_get =>  *)
	      (* | Prim.Weak_new =>  *)
	      | other => { args = arguments
			 , prim = primitive
			 , targs = type_parameters }
    in 
	primitive
    end
(* 
 * TODO(nate): we cannot just hook into primitive applications.
 * Constructor application, Tuple creation, and Tuple selection
 * SSA Expressions all require passing through the GC Barriers.
 *)

fun mapStatementPrimitiveApplications statement =
  let val Statement.T { exp, ty, var } = statement
      val exp = case exp of
		    Exp.PrimApp { args, prim, targs } =>
		    Exp.PrimApp (hookPrimitiveApplication (args, prim, targs))
		  | other => other
  in
      Statement.T { exp = exp
		  , ty = ty
		  , var = var}
  end

fun mapBlockStatements block =
    let val Block.T { args, label, statements, transfer } = block
	val statements = Vector.map (statements, mapStatementPrimitiveApplications)
    in
	Block.T { args = args
		, label = label
		, statements = statements
		, transfer = transfer }
    end

fun mapFunctionBlocks function = 
    let val { args, blocks, mayInline, name, raises, returns, start } =
	    Function.dest function
    in Function.new { args = args
		    , blocks = Vector.map (blocks, mapBlockStatements)
		    , mayInline = mayInline
		    , name = name
		    , raises = raises
		    , returns = returns
		    , start = start }
    end

fun transform program =
    let
	val Program.T { datatypes, functions, globals, main } = program
	val functions = List.revMap (functions, mapFunctionBlocks)
    in
	Program.T { datatypes = datatypes
		  , functions = functions
		  , globals = globals
		  , main = main }
    end

end
