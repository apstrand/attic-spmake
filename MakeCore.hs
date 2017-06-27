
module MakeCore where


import Maybe
import Monad
import List
import System
import MonadLib

import Path
import Misc
import Directory

import SP
import SPHelper
import Time

type Filename = String
type Path = String
type IFlag = String

data PathRel = SrcRel
             | DstRel
             | TopRel
             | Abs
 deriving (Eq, Show)

data Flag = Opt String
          | Optimize
          | Debug
          | CustomOpt
          | Profile
          | IncDir (PathRel, Path)
          | LinkDir (PathRel, Path)
          | Link String
          | PathFlag String (PathRel, Path)
          | InFile (PathRel, Path)
          | OutFile (PathRel, Path)
 deriving (Eq, Show)

type StrFn = String -> String


data ArgSpec = ArgSpec {
    as_in     :: String,
    as_out    :: String,
    as_opt    :: String,
    as_dbg    :: String,
    as_prof   :: String,
    as_incdir :: String,
    as_lnkdir :: String,
    as_link   :: StrFn,
    as_prog   :: (String,String),
    as_outfn  :: StrFn
  }

type Label = String

type ModTime = (ClockTime, String)

class Depend a where
    modtime :: a -> ModTime
    newtime :: a -> ModTime -> a
    name :: a -> [Filename]
    newname :: a -> [Filename] -> a
    flags :: a -> [Flag]
    flags a = []

data SimDepend = MkSimDepend ModTime
 deriving Show

instance Depend SimDepend where
    modtime (MkSimDepend mt) = mt
    newtime (MkSimDepend _) mt' = MkSimDepend mt'
    name _ = ["SimDepend"]
    newname mt _ = mt


data Conf = Conf {
	simulate  :: Bool,
	force	  :: Bool,
        verbosity :: Int,
        clean     :: Bool, 
        topdir    :: Path,
	srcdir    :: Path,
	dstdir    :: Path,
	gopts     :: [(Label, IFlag)],
        gflags    :: [Flag],
	env	  :: [(String, String)],
        startdir  :: Path
	}
 deriving Show


type M b c = SP (StateMonadP Conf (ErrorMonad IO)) b c

type StM a = StateMonadP Conf (ErrorMonad IO) a

msgHandler fh = getSP fh
msgHandlerNull fn fj = getRSP (maybe fn fj)


maxmodtime m fn = do
	mt <- getmodtime fn
	return (maybe m (max m) mt)



printSP :: Show a => M a a
printSP = liftMSP (\msg -> io $ putStrLn ("SP: " ++ show msg) >> return msg)


afterOne f = msgHandler
	(\src -> putSP src (putSP f idSP))

wait_for_null :: (Monad m, Depend b) => SP m SimDepend b -> SP m b b
wait_for_null = ffix far_ago (\f mt sp ->
            msgHandlerNull
              (sp <-< put1SP (MkSimDepend mt))
              (\obj -> putSP obj (f (modtime obj) sp)) )

ffix = flip fix

print_msg msgs = do
    v <- getstp verbosity
    let msg = head $ drop v ("" : msgs ++ (repeat $ last msgs))
    when (not (null msg)) $ io $ putStrLn msg

getpaths = do
    [tp,sp,dp] <- sequence [gettop,getsrc,getdst]
    return (tp,sp,dp)
    
if_mod_run mt tgt cmd cln = do
    [force, simulate, clean] <- mapM getstp [force, simulate, clean]
    paths <- getpaths
    mt' <- getmodtime tgt
    let tgt' = ccpath' paths tgt
    case (clean, force, mt') of
     (True,_,_) -> when cln $ io $ removeFile tgt' `catch` (\_ -> return ())
     (_,True,_) -> print_msg [cmd, "Forced: " ++ cmd] >> remake tgt'
     (_,_,Just mt'') -> if mt'' < mt then 
                            print_msg ["Generating: " ++ tgt', "Running: " ++ cmd,
                                       "\nDependency " ++ snd mt ++ " modified =>\n" ++ cmd, 
                                       "\nDependency modified (" ++ show mt'' ++ " < " ++ show mt ++ ") =>\n" ++ cmd] >>
                            remake tgt'
                         else print_msg ["", "Not changed: " ++ tgt', "Not changed: " ++ tgt',
                                             "Not changed: " ++ " (" ++ show mt'' ++ " >= " ++ show mt ++ ")"]
     (_,_,Nothing) -> print_msg ["Generating: " ++ tgt', "Running: " ++ cmd, 
                                 "\nNo " ++ tgt' ++ ", rebuilding =>\n" ++ cmd] >> remake tgt'
 where remake tgt = io (system cmd >>= \r ->
                        when (r /= ExitSuccess && cln)
                                (do removeFile tgt `catch` (\_ -> return ())
                                    ioError (userError cmd)))


getmodtime s = do
        paths <- getpaths
        let fn = ccpath' paths s
	io $ do
	  liftM (\a -> Just (a,fn)) (getModificationTime fn)
	 `catch` (\err -> return Nothing)

ret_last s = return (putSP s idSP)


obj2dep :: (Monad m, Depend a) => SP m a SimDepend
obj2dep = mapSP (\obj -> MkSimDepend (modtime obj))

far_ago = (toClockTime (CalendarTime 1970 January 1 1 1 1 1 Sunday 1 "GMT" 1 False), "far ago")
future  = (toClockTime (CalendarTime 2032 January 1 1 1 1 1 Sunday 1 "GMT" 1 False), "future")

accumModTime sp = ffix far_ago
    (\acc mt -> msgHandlerNull (sp mt) (\obj -> acc (max mt (modtime obj))))

--header :: Filename -> M Depend Depend
header fn = accumModTime
    (\mt -> doThenSP (maxmodtime mt (SrcRel, fn) >>= return . MkSimDepend) nullSP)

--objdep :: Filename -> M Object Object
objdep fn = msgHandler
    (\obj -> doThenSP (maxmodtime (modtime obj) (DstRel, fn) >>= \mt' ->
                       return (newtime obj mt')) nullSP)


data Source = MkSource [Flag] ModTime Filename
 deriving Show

instance Depend Source where
    newtime (MkSource fl _ fn) mt' = MkSource fl mt' fn
    modtime (MkSource _ mt _) = mt
    name (MkSource _ _ fn) = [fn]
    newname (MkSource fl mt _) (fn:_) = MkSource fl mt fn
    flags (MkSource fl _ _) = fl

source fl fn = accumModTime
    (\mt -> actionSP (maxmodtime mt (SrcRel, fn) >>=
                      \mt' -> return (put1SP (MkSource fl mt' fn) )))


object fn = actionSP $
 do mt <- getmodtime (DstRel, fn)
    return (putSP (MkObject (maybe future id mt) fn) idSP)
    

deptree gendeps a b fn = actionSP (
    do  ds <- gendeps fn
        return (a fn <-< (foldr (|*|) nullSP (map b ds))) )

ccpath = merge_paths

ccpath' (t,s,d) (r,p) = case r of
    SrcRel -> ccpath s p
    DstRel -> ccpath d p
    TopRel -> ccpath t p
    Abs    -> p


frob ac = msgHandler (\i -> doThenSP (ac i) (frob ac))

gettop :: StM String
gettop = getstp topdir
getsrc = gettop >>= \t -> getstp srcdir >>= \s -> return (ccpath t s)
getdst = gettop >>= \t -> getstp dstdir >>= \d -> return (ccpath t d)


fmtflag :: (String,String,String) -> ArgSpec -> Flag -> String
fmtflag paths as f = case f of
    Opt s -> s
    Optimize     -> as_opt as
    CustomOpt    -> ""
    Debug        -> as_dbg as
    Profile      -> as_prof as
    IncDir p     -> as_incdir as ++ ccpath' paths p
    LinkDir p    -> as_lnkdir as ++ ccpath' paths p
    Link s       -> as_link as s
    PathFlag s p -> s ++ (ccpath' paths p)
    InFile p     -> as_in as ++ ccpath' paths p
    OutFile p  -> as_out as ++ ccpath' paths p

setflag l v = modst (\st -> st { gopts = (l,v):(gopts st) } )
getflag l = liftM head $ getflags l
getflags l = do
	gs <- getstp gopts
	let filt l' = (map snd . filter (\(lbl,_) -> lbl == l'))
	    sys = maybe "gen" id (lookup "sys" gs)
	    rs = filt l gs ++ filt (sys ++ "." ++ l) gs
        if null rs then getenv l
         else return (reverse rs)



getenv :: String -> StM [String]
getenv l = io $ (liftM (\a->[a]) (getEnv l)) `catch` (\_ -> return [])

getfiltst p lbl = liftM (map snd . filter (\(label,_) -> label == lbl)) $ getstp p


xext ext = (++ ext) . fst . splitname

data Object = MkObject ModTime Filename
 deriving Show

instance Depend Object where
    newtime (MkObject _ fn) mt' = MkObject mt' fn
    modtime (MkObject mt _) = mt
    name (MkObject _ fn) = [fn]
    newname (MkObject mt _) (fn:_) = MkObject mt fn


instance Depend a => Depend [a] where
    newtime xs mt' = map (`newtime` mt') xs
    modtime xs = maximum (map modtime xs)
    name xs = concatMap name xs
    newname xs fn = map (`newname` fn) xs


data Generator a b = Generator {
        mk_cmd   :: (Path, Path, Path) -> [Flag] -> a -> ((PathRel, Filename), [String]),
        mk_out   :: ModTime -> Filename -> b,
        do_clean :: Bool
    }

exec_cmd :: String -> String -> String -> M Source Source
exec_cmd cmd inp outp = mk_generator (Generator mk_cmd (MkSource []) (inp /= outp)) <-< source [] inp
 where mk_cmd (_,sd,_) _ _ = ((DstRel, outp), [cmd,ccpath sd inp, ccpath sd outp])

mk_preprocessor :: String -> String -> M Source Source
mk_preprocessor lbl cmd = mk_generator (Generator mk_cmd (MkSource []) False)
 where mk_cmd (_,sd,_) _ inp = ((SrcRel, fn), [cmd, ccpath sd fn])
        where fn = head (name inp)

mk_preprocessor2 :: String -> (String -> (String,String,(String->String))) -> M Source Source
mk_preprocessor2 lbl f = mk_generator (Generator mk_cmd (MkSource []) False)
 where mk_cmd (_,sd,_) _ inp = ((SrcRel, outf), [cmdf (ccpath sd inf)])
        where (inf,outf,cmdf) = f (head (name inp))

copy_to_file :: Depend a => (PathRel,Path) ->  M a a
copy_to_file = copy_to (\s -> id)

copy_to_dir :: Depend a => (PathRel,Path) -> M a a
copy_to_dir = copy_to (\s d -> ccpath d s)

mkdir d = mapM_ mkdir' (splitpath d)
 where mkdir' p = catch (createDirectory p) (\_ -> return ())

copy_to tgtfn tgt = frob
 $ \inp -> do
            paths <- getpaths
            let mt = modtime inp
                src = head (name inp)
                src' = ccpath' paths (DstRel, src)
                tgt' = tgtfn src (ccpath' paths tgt)
            io $ mkdir (dirname tgt')
            if_mod_run mt (Abs, tgt') (unwords ["cp", src', tgt']) True
            mt' <- getmodtime (Abs, tgt')
            case mt' of
             Just mt'' -> return (newname (newtime inp mt'') [tgt'])
             Nothing -> return (newname (newtime inp future) [tgt'])


mk_compiler :: ArgSpec -> [Flag] -> M Source Object
mk_compiler as fs' = gen_generator as (IncDir (SrcRel,"."):fs') MkObject SrcRel True


gen_generator as fs' oo in_rel b = actionSP $ do
 let (var,pg) = as_prog as
 ep <- getflags var
 let prog = if null ep then pg else head ep
 return $ mk_generator $ Generator (cmd prog) oo b
    where cmd prog paths fs inp = ((DstRel, obj), prog : map fmt (
                                    OutFile (DstRel, obj) :
                                    map (\a -> InFile (in_rel, a)) (name inp) ++ 
                                    (fs' ++ fs)))
           where
                 obj = as_outfn as (head (name inp))
                 fmt = fmtflag paths as

mk_linker :: Depend a => ArgSpec -> String -> [Flag] -> M [a] Executable
mk_linker a b c = gen_linker a b c

outfn_out out as = as { as_outfn = (\_ -> as_outfn as out) }

gen_linker as a fs' = gen_generator (outfn_out a as) fs' MkExec DstRel True


mk_libmaker :: ArgSpec -> String -> [Flag] -> M [Object] Object
mk_libmaker as out fs'
    = gen_generator (outfn_out out as) fs' MkObject DstRel True

mk_libmaker_tmpfile :: ArgSpec -> String -> [Flag] -> M [Object] Object
mk_libmaker_tmpfile = mk_lib_tmpfile mk_libmaker MkObject

mk_linker_tmpfile :: ArgSpec -> String -> [Flag] -> M [Object] Executable
mk_linker_tmpfile = mk_lib_tmpfile mk_linker MkExec

mk_lib_tmpfile mk_next oo as out fs = getSP $ \objs ->
    actionSP $
     do	dp <- getdst
	io $ writeFile (ccpath dp "tmp_file") (unwords (map (ccpath dp) (name objs)))
	return $ mk_next (as { as_in = "@" } ) out fs <-< put1SP ([MkObject (modtime objs) "tmp_file"])
    {-
 where libmk = mk_generator $ Generator lbl cmd oo True
       cmd paths fs' _ = ((DstRel, out'), ((prog ++ fmt (OutFile (DstRel, out'))) : " @tmp_file" :
                                map fmt (fs' ++ fs)))
        where out' = as_outfn as out
              fmt = fmtflag paths as
-}

mk_libmaker_split :: ArgSpec -> String -> [Flag] -> M [Object] Object
mk_libmaker_split = mk_lib_split mk_libmaker MkObject

mk_linker_split :: ArgSpec -> String -> [Flag] -> M [Object] Executable
mk_linker_split = mk_lib_split mk_linker MkExec

wait_adddep sp as = getSP $ \o -> sp <-< put1SP (map (`newtime` (modtime o)) as)

with_force_do sp = getSP $ \objs -> actionSP $ do
    st <- getst
    setst (st { force = True })
    runSP sp [objs]
    modst (\st' -> st' { force = force st } )
    return (with_force_do sp)


if_mod_do l r t e k = actionSP $ do
    v <- getstp verbosity
    when (v > 3) $ if l < r then io $ putStrLn ("Modified: " ++ show l ++ " < " ++ show r)
                    else io $ putStrLn ("Unchanged: " ++ show l ++ " >= " ++ show r)
    return $ if l < r then t k else e k

mk_lib_split org oo as out fs' = getSP $ \objs ->
 actionSP $ if null objs then return nullSP else do
    mlmt <- getmodtime (DstRel, as_outfn as out)
    let lmt = maybe (fst far_ago, as_outfn as out) id mlmt
    return $ foldr1 (|*|) (map (\o -> mk_lib lmt <-< put1SP o) (split objs))
     where mk_lib lmt = let mk_lib_org = org as out fs'
                        in getSP $ \objs' -> ((if_mod_do lmt (modtime objs') with_force_do id) mk_lib_org) <-< put1SP objs'
           split = gen_map (splitAt magic_no)
	   magic_no = 5

merge_flags :: [Flag] -> [Flag] -> [Flag]
merge_flags xs ys = mf xs ys []
 where mf [] ys acc = ys ++ acc
       mf (x:xs) ys acc
        | x `elem` override = mf xs (filter (not . (`elem` override)) ys) (x:acc)
        | otherwise = mf xs ys (x:acc)
        where override = [Optimize,Debug,CustomOpt]
        
 

mk_generator (Generator mk_cmd mk_out do_clean) = frob
 $ \inp -> do
            paths <- getpaths
            gfls <- getstp gflags
            let fls = merge_flags (flags inp) gfls
            let
                mt = modtime inp
                ((rel, tgt), cmd) = mk_cmd paths fls inp
            if_mod_run mt (rel, tgt) (unwords cmd) do_clean
            mt' <- getmodtime (rel, tgt)
            case mt' of
             Just mt'' -> return (mk_out mt'' tgt)
             Nothing   -> return (mk_out future tgt)


data Executable = MkExec ModTime Filename
 deriving Show

instance Depend Executable where
    newtime (MkExec _ fn) mt' = MkExec mt' fn
    modtime (MkExec mt _) = mt
    name (MkExec _ fn) = [fn]
    newname (MkExec mt _) (fn:_) = MkExec mt fn

{-
mk_link prog flags out outex outfn opt2flags = frob
	 (run_gen (\_ -> out)
	 	  (\(_,p) objs tgt fls -> unwords [prog, outex, p++"/"++out,
		  		unwords (fixobjs p objs), unwords (flags ++ opt2flags fls)])
		  (\lst -> let	f (MkObject mt fl fn) = (mt,fl,fn)
		  		(ms, flagss, srcs) = unzip3 (map f lst)
				flags = concat flagss
			   in	(maximum ms, flags, srcs, outfn flags))
	)
  where	fixobjs p = map (ccpath p) . sortBy (ext_bw_ord)
-}
-- mk_linker prog flags out = mk_link prog flags out "-o" (\_ -> MkExec)

{-
mk_libmaker prog flags out = mk_link prog flags out ""
	(\flags mt tgt -> MkObject mt flags tgt)
-}


collect = ffix []
    (\f xs -> msgHandlerNull (put1SP xs) (\fn -> f (fn:xs)))

--collect_deps :: M Depend Depend -> M Depend Depend
collect_deps sp = wait_deps -- <-< (put1SP Notify `afterSP` sp)

wait_deps :: Depend a => M a SimDepend
wait_deps = accumModTime (\mt -> put1SP (MkSimDepend mt))


collect_objs :: M Object [Object]
collect_objs = ffix [] (\cc xs -> msgHandlerNull (put1SP (sort' xs)) (\x -> cc (x:xs)))

 where sort' = sortBy (\f1 f2 -> let ext = snd . splitname . head . name
                                     (x1,x2) = (ext f1, ext f2)
                                 in compare (x2, modtime f2) (x1, modtime f1))



