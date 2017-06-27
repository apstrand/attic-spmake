
module Dep where

import Array
import List
import Maybe
import System
import MonadLib
import Directory
import Monad
import MakeCore
import IO

import SP
import SPHelper
import Make
import Misc
import Path
import MakeSpec

type Deps = [(String, [String])]

cont_lines [] = []
cont_lines ('\\':'\r':'\n':xs) = ' ': cont_lines xs
cont_lines ('\\':'\n':xs) = ' ': cont_lines xs
cont_lines (x:xs) = x:cont_lines xs

parse_deps = map gen_dep . map words . dropWhile ((=='#') . head) . lines . cont_lines
	where gen_dep (w:_:ws) = (init w,ws)
	      gen_dep xs = error (show xs)

myfind t = fromJust' . find ((((==) (base t) . base)) . fst)
 where fromJust' Nothing = error ("error on Dep.hs: 26: " ++ show t)
       fromJust' (Just a) = a
myfind' t = find ((((==) (base t) . base)) . fst)

base = init . reverse . dropWhile (/='.') . reverse

merge_deps target deps = let num = length deps
			     tgt = myfind target deps
			 in rem_dups [] (reverse (merge_it tgt deps))

rem_dups qs [] = qs
rem_dups qs (x:xs)
 | x `elem` qs = rem_dups qs xs
 | otherwise   = rem_dups (x:qs) xs

merge_it (x,xs) ys = x : [ f | 
	f' <- snd (myfind x ys),
	f <- merge_it (myfind f' ys) ys
	]

gen_c_deps p target files flags = do
        let cmd = ("sh -c '(cd " ++ p ++ ";g++ -MM " ++ unwords (files) ++ ")'")
	deps <- gen_deps (p ++ "/" ++ head files) target cmd
	return deps

gen_deps src target prog = do
        mkdir ".deps"
        let depname = ".deps/" ++ (map (\c -> if c == '/' then '_' else c) target)
        (system (prog ++ " >" ++ depname) >> return ())
        deps <- readFile depname
	return (parse_deps deps)

gen_hs_deps target flags = do
	deps <- gen_deps "" target ("hmake -M " ++ flags ++ " " ++ target)
	return (merge_deps target deps)

win32_deps fn dep = do
    writeFile "dep.bat" ("@cl /nologo /D_MT /E /I. /I../build/vs/include " ++ fn ++ " > tmp_dep 2>nul")
    system ("dep.bat")
    s <- readFile "tmp_dep"
    let rmcd ('.':'\\':'\\':xs) = xs
        rmcd xs = xs
    let deps = nub . sort . map (rmcd) . filter (not . ("C:\\" `isPrefixOf`)) .
		map (takeWhile (/='"') . tail . dropWhile (/='"')) .
		filter ("#line " `isPrefixOf`) . lines
    writeFile dep (fn ++ ": " ++ unwords (deps s))

win32_mkdeps fn fl dep = do
    (t,sd,dd) <- getpaths
    io $ writeFile "dep.bat" ("@mkdep -I" ++ sd ++ " " ++ slash_path (ccpath sd fn) ++ " > " ++ ccpath dd dep)
    io $ system "dep.bat"
    return ()

pingSP = put1SP (MkSimDepend far_ago) :: M SimDepend SimDepend

gcc_deps fn fl dep = do
    st <- getst
    modst (\st -> st { force = True, verbosity = (let v = verbosity st in if v > 3 then v else 0) })
    (runG_ ((mk_compiler (gcc_as_cpl { as_out = ">", as_outfn = \_ -> dep })) [] <-< source (Opt "-MM" : fl) fn <-< pingSP))
        `handle` (io . putStrLn . ("dep generation error: "++)) 
    setst st
    return ()

ch_deps sys
 | sys == "win32-gcc" = gcc_deps
 | head sys == 'w'    = win32_mkdeps
 | otherwise          = gcc_deps

mk_depfile fn fl dn p = do
    s <- getflag "sys"
    mk_gen_depfile fn fl dn p (ch_deps s)

mk_gen_depfile fn fl dn p gen = do
    sd <- getsrc
    vb <- getstp verbosity
--    cd <- liftM slash_path $ io $ getCurrentDirectory
--    io $ setCurrentDirectory p
    when (vb > 1) (io $ putStrLn ("Generating deps for: " ++ fn))
    gen fn fl dn -- (ccpath cd dn)
--    io $ setCurrentDirectory cd


