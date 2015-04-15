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

val modularGCStateName = "modular_gc_state"
val modularGCStateVariable = Var.fromString modularGCStateName
val modularGCStateSymbol : Type.t Prim.t = 
    Prim.ffiSymbol { name = modularGCStateName
		   , cty = SOME CType.CPointer
		   , symbolScope = SymbolScope.External }

fun arrayAllocateHook type_parameter =
  CFunction.T { args = Vector.new2 ( Type.cpointer
				   , Type.word (WordSize.seqIndex ()) )
	      , convention = Convention.Cdecl
	      , kind = Kind.Impure
	      , prototype = ( Vector.new2 ( CType.cpointer
					  , CType.seqIndex () )
			    , SOME CType.objptr )
	      , return = Type.array type_parameter
	      , symbolScope = SymbolScope.External
	      , target = Target.Direct "gc_array_allocate" }

fun arraySubHook type_parameter =
  CFunction.T { args = Vector.new3 ( Type.cpointer
				   , Type.array type_parameter 
				   , Type.word (WordSize.seqIndex ()) )
	      , convention = Convention.Cdecl
	      , kind = Kind.Impure
	      , prototype = ( Vector.new3 ( CType.cpointer
					  , CType.objptr
					  , CType.seqIndex () )
			    , SOME CType.objptr )
	      , return = type_parameter
	      , symbolScope = SymbolScope.External
	      , target = Target.Direct "gc_array_access" }

fun arrayUpdateHook type_parameter =
  CFunction.T { args = Vector.new4 ( Type.cpointer
				   , Type.array type_parameter
				   , Type.word (WordSize.seqIndex ())
				   , type_parameter )
	      , convention = Convention.Cdecl
	      , kind = Kind.Impure
	      , prototype = ( Vector.new4 ( CType.cpointer
					  , CType.objptr
					  , CType.seqIndex ()
					  , CType.objptr )
			    , NONE )
	      , return = Type.unit
	      , symbolScope = SymbolScope.External
	      , target = Target.Direct "gc_array_update" }

fun hookPrimitiveApplication ( arguments, primitive, type_parameters ) =
    let val primitive = 
	    case Prim.name primitive of
		Prim.Name.Array_array => { args = Vector.new2 ( modularGCStateVariable
							      , Vector.sub (arguments, 0) )
					 , prim = Prim.ffi (arrayAllocateHook (Vector.sub (type_parameters, 0)))
					 , targs = Vector.new0 () }
	      | Prim.Name.Array_sub => { args = Vector.new3 ( modularGCStateVariable
							    , Vector.sub (arguments, 0)
							    , Vector.sub (arguments, 1) )
				       , prim = Prim.ffi (arraySubHook (Vector.sub (type_parameters, 0)))
				       , targs = Vector.new0 () }
	      | Prim.Name.Array_update => { args = Vector.new4 ( modularGCStateVariable
							       , Vector.sub (arguments, 0)
							       , Vector.sub (arguments, 1)
							       , Vector.sub (arguments, 2) )
					  , prim = Prim.ffi (arrayUpdateHook (Vector.sub (type_parameters, 0)))
					  , targs = Vector.new0 () }
	      (* TODO(nate): non-exhaustive list of primitives to implement *)
	      (* | Prim.Array_array => *)
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

fun insertModularGCDefinition program =
  let 
      val Program.T { datatypes, functions, globals, main } = program
      val definition = Statement.T { exp = Exp.PrimApp { args = Vector.new0 ()
						       , prim = modularGCStateSymbol
						       , targs = Vector.new0 () }
				   , ty = Type.cpointer
				   , var = SOME modularGCStateVariable }
      val globals = Vector.concat [Vector.new1 definition, globals]
  in
      Program.T { datatypes = datatypes
		, functions = functions
		, globals = globals
		, main = main }
  end

fun hookModularGC program =
    let
	val Program.T { datatypes, functions, globals, main } = program
	val functions = List.revMap (functions, mapFunctionBlocks)
    in
	Program.T { datatypes = datatypes
		  , functions = functions
		  , globals = globals
		  , main = main }
    end

fun transform program =
  hookModularGC (insertModularGCDefinition program)

end
