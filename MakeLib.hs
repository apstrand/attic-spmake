
module MakeLib (
	(<-<), (|*|), putList, 
	mk_preprocessor, mk_compiler, mk_linker, mk_libmaker, 
	std_compiler, std_linker, std_libmaker,
	collect_deps, collect_objs, source, header, 
	hs_compile,
	sourcedir, destdir, addflags, 
	runG, run, 
	startDep,
	simple_c_target, simple_hs_target, 
	Phase(..), Lang(..)
	) where

import SP
import SPHelper
import MakeCore
import Make
import MakeHelper


