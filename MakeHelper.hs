
module MakeHelper where

import MakeCore
import Make
import Misc
import MonadLib
import SP
import SPHelper
import Dep
import Directory

import Time
import List 
import Monad
import Maybe
import Path

import System

simple_hs_target :: String -> [Flag] -> M Object Executable
simple_hs_target name flags =
	cc_linker name [] <-< collect_objs <-< (mk_hs_sources name)



mk_hs_sources src = actionSP $ do
	deps <- io $ gen_hs_deps src []
	return (foldr1 (<-<) (map (hs_compile []) (map ch_ext deps)))
 where  ch_ext fn = (a ++ ".hs")
 	 where (a,_) = splitname fn

mk_c_sourcedeps flags src = source [] src 
	<-< mk_c_deps src flags

mk_c_deps src flgs = actionSP $ do
	p <- getsrc
	res <- io $ gen_c_deps p src [src] flgs
	case res of
	 [] -> return idSP
	 ((_,hdrs):_) -> return (foldr1 (<-<) (map header hdrs))


copy target = getSP (\(o@(MkObject _ f)) ->
	 actionSP (io $ system ("cp " ++ f ++ " " ++ target) >>
	 return (putSP o (copy target))))

exec args = getSP $ \(MkExec _ fn) ->
    actionSP (io $ system ("./"++fn++args) >> return nullSP)


runCmd cmd = do
	let fname = ".makelib.temp.file"
	system ("sh -c '(" ++ cmd ++ ")' >" ++ fname)
	liftM lines (readFile fname)

c_files_in_dir fl dir exclude = actionSP $ do
        let only_c = filter (\fn -> let (_,ext) = splitname fn in ext == "cpp" || ext == "c")
	p <- getsrc
	files <- liftM only_c $ io $ getDirectoryContents (p ++ "/" ++ dir)
	if null files then return nullSP
	 else return (foldr1 (|*|) (map (\s -> depsource fl (dir ++ "/" ++ s)) (files \\ exclude)))



c_depsource fl fn = deptree gd (source []) header fn
 where gd fn =
        do r <- getsrc >>= \dd -> io $ gen_c_deps dd fn [fn] (fl)
           return $ if null r then [] else snd (head r)


mk_depname s = ".deps/" ++ (map (\c -> if c == '/' then '_' else c) s)

fixpath [] fn = fn
fixpath (x:xs) fn
 | x `isPrefixOf` fn = fixpath xs (tail (drop (length x) fn))
 | otherwise         = fixpath xs fn

depsource :: Depend a => [Flag] -> String -> M a Source
depsource fl fn = 
    actionSP (
        do  mmt <- getmodtime (SrcRel, fn)
            let mt = maybe (error ("failed in MakeHelper.hs: 74: did not find:" ++ fn)) id mmt
            now <- io $ getClockTime
            mt' <- liftM (maybe far_ago id ) $ getmodtime (DstRel, (mk_depname fn))
            v <- getstp verbosity
            when (v > 3 && mt > mt') (io $ putStrLn ("Dep regen: " ++ show mt ++ " > " ++ show mt'))
            hdrs <- gen_hdrs (mt > mt')
            return (depsource' mt' mt fn <-< hdrs) )
 where
  depsource' mt mt' fn = accumModTime 
        (\mt'' -> actionSP $ do
                    if mt'' > mt then do
                                        v <- getstp verbosity
                                        when (v > 3) $ io $ putStrLn ("Dep regen2: " ++ show mt'' ++ " > " ++ show mt)
                                        gen_hdrs True >>= \hdrs -> return (source fl fn <-< hdrs)
                        else return $ source fl fn  <-< put1SP (MkSimDepend mt')
        )

  gen_hdrs regen = do
	    std <- getstp startdir
            (td,sd,dd) <- getpaths
            let dep_name = mk_depname fn
            let dep_file = ccpath dd dep_name
            let regenerate = mk_depfile fn fl dep_name sd
            let read_deps = io $ readFile dep_file `catch` (\_ -> return [])
            when regen regenerate
            deps <- read_deps >>= \deps ->
                     if null deps then regenerate >> read_deps
                      else return deps
            let ds = parse_deps deps
            let ds' = if null ds then [] else map (fixpath [std,sd]) (snd (head ds))
            return (foldr (|*|) nullSP (map header ds'))

mapRightSP :: M SimDepend Source -> M Object (Either Object Source)
mapRightSP sp = ((mapSP Right <-< sp <-< put1SP (MkSimDepend far_ago)) |*| mapSP Left)




