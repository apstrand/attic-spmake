
module Core where

import Monad
import Char
import List
import Time
import SP
import BSP
import ParSP
import Directory
import MonadLib
import Path

data Val
 = Real String
 | Symb String
 deriving Show


data Option
 = Opt Val
 | Arg Val String
 deriving Show

data DepInfo
 = DepInfo ClockTime
 | Epoch
 deriving Show

data Path
 = Abs String
 | Top String
 | Src String
 | Dst String
 deriving Show

data File
 = File Path DepInfo [Option]
 | Fake DepInfo [Option]

tm_of_dep (File _ (DepInfo tm) _) = tm
tm_of_dep (Fake (DepInfo tm) _) = tm

data ExecReq
 = ExecReq String [File] Path [Option]

resolvepath (Abs fn) = return fn

epoch = toClockTime $ CalendarTime 1970 January 1 0 0 0 0 Thursday 0 "GMT" 3600 False

execute :: ParSP IO ExecReq File
execute = getHSP $ \(ExecReq cmd ds op os) ->
    waitSP $ atom $ do
        fn <- resolvepath op
        tm <- getModificationTime fn `catch` (const $ return epoch)
        if any (>tm) (map tm_of_dep ds) then
          print ("running: " ++ cmd ++ "\n")
         else
          print "not modified\n"
        tm' <- getModificationTime fn
        return (putHSP (File op (DepInfo tm') os) nullSP)

remove_comments = f 0
 where f n [] = []
       f n ('{':'-':xs) = f (n+1) xs
       f n ('-':'}':xs) = f (n-1) xs
       f 0 (x:xs) = x : f 0 xs
       f n (_:xs) = f n xs

get_imports fn = do
    s <- readFile fn
    let m = map (head . dropWhile (isLower.head) . words) .
            filter ("import " `isPrefixOf`) .
            takeWhile (\s -> null s || take 7 s == "import " || not (isLower (head s))) .
            dropWhile (not . ("import " `isPrefixOf`))
    
    filterM doesFileExist (map (++".hs") (m (lines (remove_comments s))))

get_all_imports fn = do
    hs <- get_imports fn
    hss <- mapM get_all_imports hs
    return (hs:concat hss)

levelize :: [(Int,a)] -> [[a]]
levelize [] = []
levelize ((i,x):xs) = let (y,ys) = partition ((==i).fst) xs
                      in ((x:map snd y):levelize ys)

remove_dups hss = 
    let f (_,a) (_,b) = a == b
    in (reverse ((
                     levelize . nubBy f .
                     concatMap (\(i,xs) -> map ((,) i) xs) .
                     zip [1..]) (reverse hss)))

serialize = foldr (\f sp -> sp <=< f) nullSP
parallelize = foldr (\f sp -> sp ||*|| f) nullSP

hs_deps fn = do
    hss <- get_all_imports fn
    return (remove_dups hss)

hs_source fn = do
    ds <- hs_deps fn
    return (serialize (map (parallelize.map (source.Abs)) (reverse ds)))

source fn = waitSP $ atom $ do
    f <- resolvepath fn
    tm <- getModificationTime f
    return (putHSP (File fn (DepInfo tm) []) nullSP)

ghc = execute <=< mapBSP f
 where f fi@(File fn dp os) =
        let fn' = ((\(Abs fn)->Abs $ chext fn "o") fn)
            cmd = "ghc -c " ++ ((\(Abs fn) -> fn) fn)
        in ExecReq cmd [fi] fn' os

run_sp sp = unError $ runstp (runSP' sp [] []) 0



