
module Make where

import MakeCore
import Misc
import Path
import MonadLib
import SP
import SPHelper
import Directory
import IO

import List
import Monad
import Maybe
import System
import MakeSpec

linker p = cc_linker p [] <-< collect_objs
libmaker p = stat_libmaker p [] <-< collect_objs


systems = [gcc_c, gcc_cc, gcc3_c, gcc3_cc, xgcc_c, xgcc_cc, win32_gcc_c, win32_gcc_cc, 
           win32_c, win32_cc, win32_icc_c, win32_icc_cc]

with_flags f fs mk = actionSP $ do
    fs' <- liftM concat $ mapM getflags f
    mk (Opt (unwords fs'):fs)

app (f,a) = f a

c_compiler fs = with_flags ["cpl", "CFLAGS"] fs
    (\fs -> choose "c" compile >>= \cpl -> return $ cpl fs)

cc_compiler fs = with_flags ["cpl", "CXXFLAGS"] fs
    (\fs -> choose "cc" compile >>= \cpl -> return $ cpl fs)

cc_linker out fs = with_flags ["lnk", "LDFLAGS"] fs
    (\fs -> choose "cc" link >>= \lnk -> return $ lnk out fs)

stat_libmaker out fs = with_flags ["slnk"] fs 
    (\fs -> choose "cc" stat_lib >>= \lib -> return $ lib out fs)

dyn_libmaker out fs = with_flags ["dlnk", "LDFLAGS"] fs
    (\fs -> choose "cc" dyn_lib >>= \lib -> return $ lib out fs)

choose :: String -> (System -> ((b->a),b)) -> StM a
choose lang prj = do
	sys <- getsystem
	case find (\(System s l _ _ _ _) -> s == sys && l == lang) systems of
	 Nothing -> error ("unknown system or language: " ++ sys ++ " " ++ lang)
	 Just s -> return (app $ prj s)

link_in n = actionSP $
    getsystem >>= \sys ->
     case sys of
      ('g':_) -> foo (fixname "lib" ".a")
      _       -> foo (fixname "" ".lib")
 where foo fn = return $ insSP (object (fn n))


getsystem :: StM String
getsystem = liftM (fst . (break (==':'))) $ getflag "sys"


hs_as_def = ArgSpec "" "-o " "-O2" "-g" "" "-i" "-L" ("-l" ++) ("HCC","ghc")

hs_as_cpl = (hs_as_def (fixname "" ".o")) { as_in = "-c " }
hs_as_lnk = hs_as_def id

hs_as_make = (hs_as_def id) { as_out = "--make -o " }

ghc fs = mk_compiler hs_as_cpl fs
nhc fs = mk_compiler hs_as_cpl fs

ghc_lnk out fs = mk_linker hs_as_lnk out fs

hs_compiler :: [Flag] -> M Source Object
hs_compiler fs = actionSP $ do
    hcc <- getenv "HCC"
    let cpl = if null hcc then ghc
                else maybe ghc id (lookup (head hcc) [("ghc",ghc),("nhc",nhc)])
    return (with_flags ["cpl", "HSFLAGS"] fs (return.cpl))

hs_linker p fs = 
    with_flags ["lnk","HSFLAGS","HSLDFLAGS"] fs (return . mk_linker hs_as_lnk p)

hs_make_ fs src = 
    with_flags ["cpl","lnk","HSFLAGS","HSLDFLAGS"] fs
        (return . mk_linker ((hs_as_def (\_->"")) { as_out="--make " }) "") <-< collect_objs <-< putSP (MkObject future src) idSP

hs_make p fs src = 
    with_flags ["cpl","lnk","HSFLAGS","HSLDFLAGS"] fs
        (return . mk_linker hs_as_make p) <-< collect_objs <-< putSP (MkObject future src) idSP

hs_compile fs s = wait_for_null (hs_compiler fs <-< source [] s)

stdconf = Conf False False 1 False "." "." "." [] [] [] "."

sourcedir d = mod_dir_help srcdir (\n st -> st { srcdir = n }) d
destdir   d = mod_dir_help dstdir (\n st -> st { dstdir = n }) d
rootdir   d = mod_dir_help topdir (\n st -> st { topdir = n }) d

mod_dir_help sel upd d = do
    old <- getstp sel
    modst (upd (either (if old == "." then id else (\_->old)) id d))


fixfl (lg,fl) = (\l g -> lg == (l,g), [fl])


modarg a st = case a of
             "-q" -> st { verbosity = pred (verbosity st) }
             "-c" -> st { clean = True }
             "-f" -> st { force = True }
             "-s" -> st { simulate = True }
             "-v" -> st { verbosity = succ (verbosity st) }
             "-o" -> st { gflags = Optimize : (gflags st) }
             "-g" -> st { gflags = Debug : (gflags st) }
             "-p" -> st { gflags = Profile : (gflags st) }
             ['-',f] -> error ("Unknown flag: " ++ [f])
             ('-':a:as) -> modarg ('-':[a]) (modarg ('-':as) st)
             _    -> case break (=='=') a of
                      (_,[]) -> st { gopts = ("target", a) : (gopts st) }
                      (var,'=':val) -> case lookup var [("srcdir", \st a -> st { srcdir = a }),
                                                        ("topdir", \st a -> st { topdir = a }),
                                                        ("dstdir", \st a -> st { dstdir = a })] of
                                        Just fn -> (fn st val)
                                        Nothing -> st { gopts = (var, val) : gopts st }
                                        
                                                    
                      _ -> st

setarg a = modst $ modarg a

startDep = nullSP

run ts = do
    conf <- initialize
    let restore = setCurrentDirectory (startdir conf)
    res <- unError (runstp (run_targets ts) conf)
    restore
    case res of
     Right res -> return res
     Left err  -> ioError (userError err)

run_targets targets = do
    ts <- getflags "target"
    sequence_ [a | Just a <- map (`lookup` targets) ts]

runG :: (Monad m) => SP m SimDepend c -> m [c]
runG g = runSP g []

runG_ g = runG g >> return ()

putTab = putStrLn . ((++) "\t")


usage targets = do
        putStrLn "Reaper [-q] [-v*] [-c] [-s] [-o] [-g] [-p] <system> <target>"
	putStrLn "Flags:"
	mapM_ putTab ["-q  Quiet", "-v  Verbose", "-c  Clean", "-s  Simulate",
                      "-o  Optimize", "-g  Debug", "-p  Profile"]
	putStrLn "Systems:"
	mapM_ putTab (nub (sort (map sys systems)))
	putStrLn "Targets:"
	mapM_ (putTab . fst) targets
	return ()

initialize = do
    hSetBuffering stdout LineBuffering
    let get a = catchMaybe (getEnv a >>= \b -> return (a,b))
    [td,sd,dd] <- mapM get ["TOP", "SRC", "DEST"]
    args <- getArgs
    foo <- getProgName
    let bar = reverse . dropWhile (\c -> c /= '/' && c /= '\\') . reverse $ foo
    cwd <- getCurrentDirectory
    let td' = if cwd == (take (length cwd) bar) then "./" else bar
    let stdconf' = stdconf { startdir = cwd,
                             srcdir = maybe "." snd sd,
                             topdir = maybe td' snd td,
                             dstdir = maybe "." snd dd }
    return (foldl (flip modarg) stdconf' args)



serialize sp xs = foldr (<-<) nullSP (map sp xs)
parallelize sp xs = foldr (|*|) nullSP (map sp xs)




