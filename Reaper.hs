

module Main where

import SP
import SPHelper
import MonadLib
import MakeCore
import Make
import Dep
import MakeHelper
import MakeSpec
import Misc
import System
import List
import Maybe
import IO
import Path

import Monad



data TargetConf = TC {
        tc_arch :: String,
        tc_compile_flags :: [Flag],
        tc_link_flags :: [Flag],
        tc_extra_in_os :: [(String, [Flag])],
        tc_plugins :: [TargetConf -> M Object Object],
        tc_opt_plugins :: [TargetConf -> M Object Object]
    }


-----

win32_conf = TC "win32"
    [Opt "/W3", Opt "/MD", IncDir (TopRel, "build/win32-support/include")]
    (LinkDir (DstRel, ".") : LinkDir (TopRel, "build/win32-support/lib") : map Link ["user32", "gdi32", "opengl32", "glu32", "libpngz"])
    [ (fn,[]) | fn <- ["conc_win32.cpp", "win32_errors.cpp", "videomode.cpp", "event/event_prim.cpp", "time_types.cpp"] ]
    [gfx_win32, event_win32 'w', snd_dummy]
    w_plugins

linux_conf = TC "linux"
    [Opt "-Wall -W -ansi -Wno-sign-compare -Wno-unused"]
    (LinkDir (DstRel, ".") : LinkDir (Abs, "/usr/X11R6/lib") :  map Link ["GL", "GLU", "X11", "pthread", "png", "z", "dl"])
    (("conc_pthread.cpp", [])
        : [(fn,[Opt "-fPIC"]) | fn <- ["videomode.cpp", "event/event_prim.cpp", "time_types.cpp"] ] )
    [gfx_x11, event_x11, snd_dummy]
    ((\c -> plugin c "os/snd/snd_openal.cpp" [Link "openal"] []) : l_plugins)

linux_conf_softgl = TC "linux"
    [Opt "-Wall -W -ansi -Wno-sign-compare -Wno-unused"]
    (LinkDir (DstRel, ".") : LinkDir (Abs, "/usr/X11R6/lib") :  map Link ["GL-static", "GLU", "X11", "Xext", "pthread", "png", "z", "dl"])
    (("conc_pthread.cpp", [])
        : [(fn,[Opt "-fPIC"]) | fn <- ["videomode.cpp", "event/event_prim.cpp", "time_types.cpp"] ] )
    [gfx_x11, event_x11, snd_dummy]
    ((\c -> plugin c "os/snd/snd_openal.cpp" [Link "openal"] []) : l_plugins)

bsd_conf = TC "freebsd"
    [Opt "-Wall -W -ansi -Wno-unused -Wno-non-virtual-dtor -Wno-sign-compare"]
    (LinkDir (DstRel, ".") :  LinkDir (Abs, "/usr/X11R6/lib") : map Link ["GL", "GLU", "lthread", "png", "z"])
    [("conc_pthread.cpp", [])]
    [gfx_x11, event_x11, snd_dummy]
    l_plugins

solaris_conf = linux_conf {
      tc_arch = "solaris",
      tc_link_flags = map Link ["m", "resolv", "nsl", "rt", "socket"] ++ tc_link_flags linux_conf
    }


xwin32_conf = TC "win32"
    [IncDir (TopRel, "build/win32-support/include")]
    (LinkDir (DstRel, ".") : LinkDir (TopRel, "build/win32-support/lib") : map Link ["png", "z", "user32", "gdi32", "winmm", "opengl32", "glu32", "wsock32"])
    [ (fn, []) | fn <- ["conc_win32.cpp", "win32_errors.cpp", "videomode.cpp", "event/event_prim.cpp", "time_types.cpp" ] ]
    [gfx_win32, event_win32 'g', snd_dummy]
    x_plugins


-----

w_plugins =
    (\c -> plugin c "os/snd/snd_openal.cpp" [LinkDir $ (TopRel, "build/win32-support/lib"), Link "OpenAL32"] []) :
    snd_mp3 : 
    snd_dsound : common_plg

l_plugins =
--    gfx_sdl : event_sdl :
    snd_mp3 : 
    common_plg

x_plugins =
    snd_dsound : common_plg


x11_flags = Opt "-L/usr/X11R6/lib -lX11 -DXF86VM_EXT"

common_plg = [snd_simple, snd_wave]

event_x11 tc = plugin tc "os/event/event_x11.cpp" [x11_flags] ["os/event/event_prim.cpp", "os/time_types.cpp"]
gfx_x11 tc = plugin tc "os/gfx/gfx_x11.cpp" [x11_flags] ["os/videomode.cpp"]
gfx_win32 tc = plugin tc "os/gfx/gfx_win32.cpp" [] ["os/videomode.cpp", "misc/parse.cpp"]
snd_dsound tc = plugin tc "os/snd/snd_dsound.cpp" [Link "dsound", Link "dxguid"] ["os/win32_errors.cpp"]
snd_wave tc = plugin tc "os/snd/snd_wave.cpp" [] []
snd_dummy tc = plugin tc "os/snd/snd_dummy.cpp" [] []

snd_simple tc =
    plugin tc "os/snd/snd_simple.cpp" [] ["os/snd/snd_simple_impl.cpp", "os/time_types.cpp"]

event_win32 c tc = 
    plugin tc "os/event/event_win32.cpp" [Link "dinput", Link "dxguid"]
                ["os/event/event_prim.cpp", "os/time_types.cpp", "os/win32_errors.cpp"]
  <-< (c_compiler [] <-< depsource [] "os/dxwrap.c")

gfx_sdl tc = plugin tc "os/gfx/gfx_sdl.cpp" [] []
event_sdl tc = plugin tc "os/event/event_sdl.cpp" [] []

plg_flg tc = case tc_arch tc of
    'w':_ -> []
    _     -> [Opt "-fPIC"]

snd_mp3 tc = plugin tc "os/snd/snd_mp3.cpp" [] [] <-< objs_mp3 tc

mk_shared_obj tc name fs =
    dyn_libmaker name (fs ++ tc_link_flags tc) <-< collect_objs <-< (idSP |^| compiler tc)

plugin tc n fs xsrc =
        copy_to_file (TopRel, "plugins/" ++ (tc_arch tc) ++ "/" ++ (basename n) ++ ".rp")
    <-< mk_shared_obj tc (basename n) fs
    <-< mapRightSP (parSP (depsource []) (n:"main/exceptions.cpp":xsrc))

{-
plugin tc n fs xsrc =
        copy_to_file (TopRel, "plugins/" ++ (tc_arch tc) ++ "/" ++ rp_name ++ ".rp")
    <-< dyn_libmaker n (fs ++ tc_link_flags tc)
    <-< collect_objs <-< (((compiler tc_plg <-<
                            foldr1 (|*|) (map (depsource []) (n:x:xsrc)))
                           <-< put1SP (MkSimDepend far_ago)) |*| idSP)
 where rp_name = (reverse . takeWhile (/='/') . tail . dropWhile (/='.') . reverse) n
       tc_plg = tc { tc_compile_flags = (Opt "-fPIC") : (tc_compile_flags tc) }
-}

compiler tc = actionSP $ do
 s <- getflag "sys"
 return $ cc_compiler (tc_compile_flags tc)
   <-< case s of
--        "gcc3" -> mapSP (no_opt "world/world.cpp")
        "gcc3" -> mapSP (no_opt "gfx/gfx_render.cpp")
        "gcc"  -> mapSP (no_opt "res/resource.cpp" . no_opt "game/menus.cpp")
        _ -> idSP

no_opt fn src@(MkSource fl mt fn')
 | fn == fn' = MkSource (CustomOpt:fl) mt fn'
 | otherwise = src

subsystems = [
        ("ai", ["steering.cpp"]),
        ("game", []),
        ("net", []),
        ("object", []),
        ("phys", ["test_phys.cpp", "test_phys2.cpp", "test_phys4.cpp", "test_phys5.cpp"]),
        ("res", []),
        ("snd", []),
        ("world", []),
        ("gfx", []),
        ("gfx/cloud", []),
        ("gfx/shadow", []),
        ("gfx/pm", ["test_stuff.cpp"]),
	("hw", ["win32_errors.cpp", "conc_win32.cpp", "conc_pthread.cpp",
		"gfx_win32.cpp", "event_win32.cpp",
		"gfx_x11.cpp", "event_x11.cpp", "gfx_sdl.cpp",
		"snd_dummy.cpp", "snd_openal.cpp", "snd_simple.cpp",
		"snd_dsound.cpp", "snd_simple_impl.cpp", "video2.cpp",
                "video.cpp", "video_helper.cpp", "videomode.cpp", "event_prim.cpp",
                "event_sdl.cpp", "snd_wave.cpp", "snd_mp3.cpp", "dxwrap.c", "time_types.cpp"]),
        ("os", ["conc_win32.cpp", "conc_pthread.cpp", "win32_errors.cpp", "dxwrap.c"]),
        ("os/event", ["event_win32.cpp", "event_x11.cpp", "event_sdl.cpp"]),
        ("os/gfx", ["gfx_win32.cpp", "gfx_x11.cpp", "gfx_sdl.cpp"]),
        ("os/gl", []),
        ("os/net", []),
        ("os/snd", ["snd_mp3.cpp", "snd_dsound.cpp", "snd_dummy.cpp", "snd_openal.cpp",
                    "snd_simple.cpp", "snd_simple_impl.cpp", "snd_wave.cpp"]),
        ("msg", []),
	("misc", ["tmpl.cpp"]),
        ("main", [])
        ]

plugindir a = "../../../plugins/" ++ a ++ "/"

libreaper tc = stat_libmaker "reaper" [] <-< collect_objs
    <-< compiler tc <-< (parallelize (\(d,e) -> c_files_in_dir [] d e) subsystems
                    |*|  parallelize (\(s,fs) -> depsource fs ("os/" ++ s)) (tc_extra_in_os tc))


rp_dll tc = copy_to_file (TopRel, "plugins/" ++ (tc_arch tc) ++ "/" ++ "reaper.rp")
    <-< dyn_libmaker "reaper" (tc_link_flags tc) <-< collect_objs <-< link_in "reaper"
    <-< compiler tc <-< depsource [] ("game/forked.cpp")

reaper_prog tc ls bin srcs = copy_to_dir (DstRel, "../bin/")
    <-< cc_linker bin (tc_link_flags tc ++ ls ++ if head (tc_arch tc) == 'g' then [Opt "-rdynamic"] else [])
    <-< collect_objs <-< link_in "reaper"
    <-< compiler tc <-< either (parallelize (depsource [])) id srcs


test tc = actionSP $ do
    src <- getflag "src"
    let bin = (flatten_name . fst . splitname) src
    f <- getstp force
    return (reaper_prog tc [] bin (Left [src]))

cpl tc = actionSP $ do
    src <- getflag "src"
    return (compiler tc <-< depsource [] src)

proto tc = reaper_prog tc [] "prototype" (Left ["public/prototype/prototype.cpp"])

ghc_strange_flags = concat $ 
  "   -lHOpenGL -lHSdata -lHSutil -lHSutil_cbits -lHSposix -lHSposix_cbits -lHSconcurrent -lHSlang -lHSlang_cbits"
 :"   -lHShaskell98 -lHSbase -lHSbase_cbits -lHSrts -lm -lgmp -ldl "
 :"   -u GHCziBase_Izh_static_info -u GHCziBase_Czh_static_info"
 :"   -u GHCziFloat_Fzh_static_info -u GHCziFloat_Dzh_static_info"
 :"   -u GHCziPtr_Ptr_static_info -u GHCziWord_Wzh_static_info"
 :"   -u GHCziInt_I8zh_static_info -u GHCziInt_I16zh_static_info"
 :"   -u GHCziInt_I32zh_static_info -u GHCziInt_I64zh_static_info"
 :"   -u GHCziWord_W8zh_static_info -u GHCziWord_W16zh_static_info"
 :"   -u GHCziWord_W32zh_static_info -u GHCziWord_W64zh_static_info"
 :"   -u GHCziStable_StablePtr_static_info -u GHCziBase_Izh_con_info"
 :"   -u GHCziBase_Czh_con_info -u GHCziFloat_Fzh_con_info"
 :"   -u GHCziFloat_Dzh_con_info -u GHCziPtr_Ptr_con_info"
 :"   -u GHCziStable_StablePtr_con_info -u GHCziBase_False_closure"
 :"   -u GHCziBase_True_closure -u GHCziPack_unpackCString_closure"
 :"   -u GHCziIOBase_stackOverflow_closure -u GHCziIOBase_heapOverflow_closure"
 :"   -u GHCziIOBase_NonTermination_closure -u GHCziIOBase_BlockedOnDeadMVar_closure"
 :"   -u GHCziIOBase_Deadlock_closure -u GHCziWeak_runFinalizzerBatch_closure"
 :"   -u __stginit_Prelude -u Main_zdmain_closure"
 :[]

hs_flgs tc = 
 tc { tc_link_flags = LinkDir (DstRel, ".") : Opt "build/ext_hs_PlantFFI.o build/PlantFFI_stub.o -L/usr/lib/ghc-5.04.2 " :
                      Link "plant" : Opt ghc_strange_flags : tc_link_flags tc,
      tc_compile_flags = IncDir (SrcRel, "ext/hs") : IncDir (Abs, "/usr/lib/ghc-5.04.2/include") : tc_compile_flags tc }


glut_lib ('w':_) = Link "glut32"
glut_lib _ = Link "glut"

leveledit tc = reaper_prog (tc { tc_compile_flags = [] } ) [Link "glui", glut_lib (tc_arch tc)] "level_editor"
    (Left $ map ("tools/level_editor/"++) ["editor.cpp", "misc.cpp", "globals.cpp", "callbacks.cpp", "gui.cpp"])


--wx_libs ('l':_) = [Link "`wx-config --libs --gl-libs"]
wx_libs _ = [Link "wx_gtk-2.2", Link "wx_gtk_gl-2.2"]

wx_flags _ = [Opt "`wx-config --cxxflags`"]

gui tc = reaper_prog (tc { tc_compile_flags = [] } ) (wx_libs tc) "reaper-gui"
            (Right (c_files_in_dir (wx_flags tc) "tools/gui" []))
    

libglui tc = stat_libmaker "glui" [] <-< collect_objs
        <-< compiler tc
        <-< c_files_in_dir [] "ext/glui"
                ["example1.cpp", "example2.cpp",
                 "example3.cpp", "example4.cpp",
                 "example5.cpp", "ppm2array.cpp"]

objs_mp3 tc = compiler tc <-< c_files_in_dir (plg_flg tc) "ext/mpegsound" []


mpegvideo_files = map ("ext/mpegvideo/"++)
    ["decoder", "input", "mpegplay", "mpgplayer", "output",
     "splay", "util", "util/render", "util/render/dither"]

libmpeg tc = stat_libmaker "mpeg" [] <-< collect_objs
    <-< compiler tc' <-< (parallelize (\xs -> c_files_in_dir [] xs []) mpegvideo_files)
 where tc' = tc { tc_compile_flags = IncDir (SrcRel, "ext/mpegvideo") : tc_compile_flags tc }


mapM_mayfail_ f = mapM_ (\x -> f x `handle` (\err -> io $ putStrLn ("Ignored failure: " ++ err)))

mk_plg tc p = runG_ (p tc <-< nullSP)

plugins tc = actionSP $ do
        mapM_ (mk_plg tc) (tc_plugins tc)
        mapM_mayfail_ (mk_plg tc) (tc_opt_plugins tc)
        return nullSP

{-
conv tgt = actionSP $ do
 cwd <- topdir
 let cmd = cwd ++ ("/build/spmake/conv.sh " ++ tgt)
 liftMSP (\fn -> io (system cmd))

pack fn = printSP

list_textdata = putList ["foo"] nullSP
list_bindata  = putList [] nullSP


list_datafiles = actionSP $ do
    return (putList [] nullSP)

mk_dist_data = do
    [tgt] <- getflag "tgt"
    ver' <- getflag "ver"
    let ver = case ver' of (x:_) -> x
                           _ -> "latest"
    return $ pack ("reaper-" ++ ver ++ "-data") <-< collect
        <-< ((conv tgt <-< list_textdata)
             |*| list_bindata)
-}

do_init args = do
    mapM setarg ((words args))
    getflags "sys"

{-
run_r sp = do
    sys <- getenv "sys"
    case sys of
     [s] -> runG_ (sp (sys2tc s)) >> return ()
     _ -> io $ usage
-}
run_r :: 
    (TargetConf -> SP (StateMonadP Conf (ErrorMonad IO)) SimDepend c)
    -> StateMonadP Conf (ErrorMonad IO) ()
run_r sp = do
    sys <- getflags "sys"
    rootdir (Right "../reaper")
    destdir (Right "../spmake/build")
    sourcedir (Right "src")
    case sys of
     [s] -> runG_ (sp (sys2tc s))
     _ -> io $ putStrLn ("usage: ./Reaper <target>")

plant_lib _ = 
   hs_compiler [Opt "-package gtkhs -package HOpenGL -package lang -i../plant -ffi -odir build -hidir ../reaper/src/ext/hs"] <-<
   source [] "ext/hs/PlantFFI.hs"

run_p sp = do
    rootdir (Right "../plant")
    sourcedir (Right ".")
    destdir (Right "../spmake/build")
    runG_ (sp <-< nullSP)

hs_fs = map Opt ("-fglasgow-exts":"-fallow-overlapping-instances":pkgs)
     ++ map Opt ["'-#include <gtkgl/gtkglarea.h>'","'-#include <libglade-1.0/glade/glade.h>'"]
     ++ map Link ["png","gtkgl","gthread","gtk","gdk","glib"]
 where pkgs = map ("-package "++) ["data","util","gtkhs","HOpenGL"]

hs_plant_objects = [
    "../MonadLib.o", 
    "../../plant/IdentMonad.o", 
    "../../plant/DeepSeq.o", 
    "../../plant/Types.o", 
    "../../plant/Property.o", 
    "../Misc.o", 
    "../../plant/LinAlg.o", 
    "../../plant/Pf.o", 
    "../../plant/ApproxTree.o", 
    "../../plant/GeomRep.o", 
    "../../plant/SimEngine.o", 
    "../../plant/Seed2.o", 
    "../../plant/IdentMonad.o",
    "../../plant/LSystem.o",
    "../../plant/FuncExpr.o",
    "../../plant/Simul2.o", 
    "../../plant/PlantTypes.o", 
    "../../plant/Draw2.o", 
    "../../plant/EngineInst2.o", 
    "../../plant/Plants2.o", 
    "../../plant/Simple.o", 
    "../../plant/LSystem.o", 
    "../../plant/NonBasic.o", 
    "../../plant/Helpers.o", 
    "../../plant/MetaLibLow.o", 
    "../../plant/MetaLibHigh.o", 
    "../../plant/Meta.o", 
    "../../plant/MetaPlants.o", 
    "../../plant/STArrLib.o", 
    "../../plant/BArrAtoms.o", 
    "../../plant/SimStateHelpers.o", 
    "../../plant/Wither.o", 
    "../../plant/MonInter.o", 
    "../../plant/SimStateIHelpers.o", 
    "../../plant/Draw.o", 
    "../../plant/Run.o", 
    "../../plant/EngineInst1.o", 
    "../../plant/WrapParam.o", 
    "../../plant/Plants1.o", 
    "../../plant/NoLoadModule.o",
    "../../plant/GLHelper.o",
    "../../plant/GLMisc.o",
    "../../plant/GLView.o",
    "../../plant/Gran.o",
    "../../plant/Texture.o",
    "../../plant/NV_Evals.o",
    "../../plant/Png.o",
    "../../plant/png/png.o",
    "../../plant/MultiTexturing.o"
 ]

plantlab = 
  stat_libmaker "plant" [] <-< collect_objs
      <-< parallelize object hs_plant_objects
      <-< wait_then_null <-< hs_make "NoLoadModule.o" (Opt "-i../plant":hs_fs) "../../plant/NoLoadModule.hs" <-< object "png/png.o"

wait_then_null = getSP (const nullSP)

targets = [("lib", run_r libreaper),
	   ("plugins", run_r plugins),
	   ("test", run_r test ),
	   ("prototype", run_r proto),
           ("leveleditor", (run_r libglui >> run_r leveledit)),
           ("dll", run_r rp_dll),
           ("gui", run_r gui),
           ("cpl", run_r cpl),
           ("all_plants", sequence_ [run_p plantlab, run_r plant_lib, run_r (libreaper.hs_flgs), run_r plugins, run_r (proto.hs_flgs)]),
           ("all", sequence_ [run_r libreaper, run_r plugins, run_r proto]) ]
--           ("dist_data", runG_ . mk_dist_data) ]

sys2tc s = let l = zip ["gcc", "gcc3", "gcc3p"] (repeat linux_conf) ++
                   [("gcc3:softgl", linux_conf_softgl)] ++
                   [("gcc:fbsd", bsd_conf)] ++
                   [("gcc:sol", solaris_conf)] ++ 
                   zip ["win32", "win32-icc"] (repeat win32_conf) ++
                   zip ["xgcc", "cyg-win32", "win32-gcc"] (repeat xwin32_conf)
           in fromJust (lookup s l)


main = run targets

