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

structure CType = CFunction.CType

fun dummyLogHook () = 
    CFunction.T { args = Vector.new1 ( CType.gcState () )
		, convention = Cdecl
		, kind = Kind.Runtime { bytesNeeded = NONE
				      , ensureBytesFree = false
				      , mayGC = false
				      , maySwitchThreads = false
				      , modifiesFrontier = false
				      , readsStackTop = true
				      , writesStackTop = false }
		, prototype = ( Vector.new1 ( Type.gcState () )
			      , NONE )
		, return = Type.unit
		, symbolScope = SymbolScope.Private
		, target = Target.Direct "gc_log" }

fun arrayAllocateHook return =
    CFunction.T { args = Vector.new3 ( CType.gcState ()
				     , CType.csize ()
				     , CType.objptrHeader () )
		, convention = Cdecl
		, kind = Kind.Runtime { bytesNeeded = NONE
				      , ensureBytesFree = true
				      , mayGC = true
				      , maySwitchThreads = false
				      , modifiesFrontier = true
				      , readsStackTop = true
				      , writesStackTop = true }
		, prototype = ( Vector.new3 ( CType.gcState ()
					    , CType.csize ()
					    , CType.objptrHeader () )
			      , SOME CType.objptr )
		, return = return
		, symbolScope = SymbolScope.Private
		, target = Target.Direct "gc_allocate_array" }

fun hookPrimitiveApplication ( arguments, primitive, argumentTypes ) =
    let val primitive = 
	    case primitive of
		Prim.Array_array => { args = Vector.sub (arguments, 0)
				    , prim = Prim.FFI dummyLogHook ()
				    , targs = Type.gctate () }
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
	      | other => other
    in 
	primitive
    end
(* 
 * TODO(nate): we cannot just hook into primitive applications.
 * Constructor application, Tuple creation, and Tuple selection
 * SSA Expressions all require passing through the GC Barriers.
 *)

fun mapStatementPrimitiveApplications (statement, transform) =
    let val Statement.T { expression, ty, var } = statement
	val expression = case expression of
			     Exp.PrimApp { args, prim, targs } => transform ( args, prim, targs )
			   | other => other
    in
	Statement.T { exp = expression
		    , ty = ty
		    , var = var}
    end

fun mapBlockStatements (block, transform) =
    let val Block.T { args, label, statements, transfer } = block
	val statements = Vector.map (statements, transform)
    in
	Block.T { args = args
		, label = label
		, statements = statements
		, transfer = transfer }
    end

fun mapFunctionBlocks (function, transform) = 
    let val { args, blocks, mayInline, name, raises, returns, start } =
	    Function.dest function
    in Function.new { args = args
		    , blocks = Vector.map (blocks, transform)
		    , mayInline = mayInline
		    , name = name
		    , raises = raises
		    , returns = returns
		    , start = start }
    end

fun transform program =
    let
	val Program.T { datatypes, functions, globals, main } = program
	val functions = List.revMap (functions, transformFunction)
    in
	Program.T { datatypes = datatypes
		  , functions = functions
		  , globals = globals
		  , main = main }
    end

end
