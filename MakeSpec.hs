
module MakeSpec where

import MakeCore
import Path



type Compiler = ArgSpec -> [Flag] -> M Source Object
type LibMaker = ArgSpec -> String -> [Flag] -> M [Object] Object
type Linker   = ArgSpec -> String -> [Flag] -> M [Object] Executable

data System = System {
	sys      :: String,
	lang     :: String,
	compile  :: (Compiler, ArgSpec),
	link     :: (Linker, ArgSpec),
	stat_lib :: (LibMaker, ArgSpec),
	dyn_lib  :: (LibMaker, ArgSpec)
    }


gcc_as_def = ArgSpec "" "-o "  "-O2" "-g" "-pg" "-I" "-L" ("-l" ++)

gcc_as_cpl      = (gcc_as_def ("CC","gcc") (fixname "" ".o")) { as_in = "-c " }
gcc_as_link     = gcc_as_def ("CC","gcc") id
gcc_as_slib     = (gcc_as_def ("AR", "ar rcs ") (fixname "lib" ".a")) { as_out = "", as_opt = "", as_dbg = "", as_prof = "" }
gcc_as_dlib     = (gcc_as_def ("CC","gcc") (fixname "lib" ".so")) { as_out = "-shared -o " }


fixname pre post = flatten_name . (pre++) . (++post) . fst . splitname

with_cc1 cc org = org {
    compile = (mk_compiler, (snd (compile org)) { as_prog = cc } )
 }

with_cc cc org = org {
    compile = (mk_compiler, (snd (compile org)) { as_prog = cc } ),
    link    = (mk_linker,   (snd (link org))    { as_prog = cc } ),
    dyn_lib = (mk_libmaker, (snd (dyn_lib org)) { as_prog = cc } )
 }


gcc_c = System "gcc" "c"
    (mk_compiler, gcc_as_cpl)
    (mk_linker, gcc_as_link)
    (mk_libmaker_split, gcc_as_slib)
    (mk_libmaker, gcc_as_dlib)

gcc_cc = with_cc ("CXX", "gcc") gcc_c { lang = "cc" }

gcc3_c  = with_cc ("CC",  "gcc-3.0") gcc_c { sys = "gcc3" }
gcc3_cc = with_cc ("CXX", "g++-3.0") gcc_cc { sys = "gcc3" }

gcc_cyg_c  = gcc_c  { sys = "cyg-win32", compile = (\as fs -> mk_compiler as (Opt "-fvtable-thunks":fs), gcc_as_cpl) }
gcc_cyg_cc = with_cc ("CXX", "g++") gcc_cyg_c


win32_as_def = ArgSpec "" "/Fo" "/G6 /O2" "/D_DEBUG /GZ /Zi" ""   "/I" "/libpath:" (++ ".lib")

win32_as_cpl  = (win32_as_def ("CC","cl /nologo")            (fixname "" ".obj")) { as_in = "/c " }
win32_as_link = (win32_as_def ("LD","link /nologo")          (fixname "" ".exe")) { as_out = "/out:", as_opt = "", as_dbg = "/DEBUG" }
win32_as_slib = win32_as_link { as_prog = ("AR","link /lib /nologo "), as_outfn = fixname "" ".lib" }
win32_as_dlib = win32_as_link { as_prog = ("DYNLD","link /dll /nologo /nodefaultlib:libc"), as_outfn = fixname "" ".dll" }


gcc_cross = "i586-mingw32msvc-gcc"
gpp_cross = "i586-mingw32msvc-g++"
ar_cross  = "i586-mingw32msvc-ar"


xgcc_c = System "xgcc" "c"
    (mk_compiler,       gcc_as_cpl  { as_prog = ("CC", gcc_cross) })
    (mk_linker,         gcc_as_link { as_prog = ("CC", gcc_cross),        as_outfn = as_outfn win32_as_link })
    (mk_libmaker_split, gcc_as_slib { as_prog = ("AR", ar_cross ++ " rcs "),      as_outfn = as_outfn win32_as_slib } )
    (mk_libmaker,       gcc_as_dlib { as_prog = ("DYNLD", gpp_cross ++ " -shared "), as_outfn = as_outfn win32_as_dlib } )

xgcc_cc = with_cc ("CXX", gpp_cross) xgcc_c { lang = "cc" }


win32_gcc_c = System "win32-gcc" "c"
    (mk_compiler,       gcc_as_cpl)
    (mk_linker,         gcc_as_cpl  { as_outfn = as_outfn win32_as_link } )
    (mk_libmaker_split, gcc_as_slib { as_outfn = as_outfn win32_as_slib } )
    (mk_libmaker,       gcc_as_dlib { as_outfn = as_outfn win32_as_dlib } )

win32_gcc_cc = with_cc ("CXX", "g++") win32_gcc_c { lang = "cc" }


win32_c = System "win32" "c"
    (mk_compiler,       win32_as_cpl)
    (mk_linker_tmpfile, win32_as_link)
    (mk_libmaker_tmpfile, win32_as_slib)
    (mk_libmaker_tmpfile, win32_as_dlib)

win32_cc = with_cc1 ("CXX","cl /nologo /GR /GX") win32_c { lang = "cc" }

win32_icc_c  = with_cc1 ("CC","icl") win32_cc { sys = "win32-icc" }
win32_icc_cc = with_cc1 ("CXX","icl") win32_icc_c



