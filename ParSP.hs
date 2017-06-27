
module ParSP where

import SP
import SPHelper
import BSP

import Directory
import System
import MonadLib

import IOExts

data Req
    = Wait (IO Bool)
    | ReqId

instance Show Req where
    show (Wait _) = "Wait"
    show ReqId = "ReqId"

data Resp
    = Done
    | NextId Int
 deriving Show

type ParSP m i o = BSP m i o Resp Req

data Action m a
 = Atom (m (Action m a))
 | Return a

atom m = ContMonad (\k -> Atom (m >>= \a -> return (k a)))
wait w = ContMonad $ \k ->
    Return $ putLSP (Wait w) $ getLSP $ \Done->
            actionSP (run_ac (k nullSP))

run_ac (Atom m) = m >>= \a -> run_ac a
run_ac (Return m) = return m

waitSP m = actionSP $ run_ac $ unCM m (\v->(Return v))

putLSP l sp = putSP (Low ([],l)) sp

getHSP f = getSP $ \msg -> case msg of High h -> f h ; _ -> getHSP f
putHSP h sp = putSP (High h) sp

getLSP f = getSP $ \msg -> case msg of Low (_,l) -> f l ; _ -> getLSP f

(||*||) sp1 sp2 = mapBSP f <=< (sp1 ||+|| sp2) <=< g
 where f (Left a) = a
       f (Right a) = a
       g = getHSP (\a -> putHSP (Left a) (putHSP (Right a) g))

getIdSP = putSP (Low ([],ReqId)) wait
 where wait = getLSP $ \(NextId i) -> putHSP i nullSP

systemSP :: String -> ParSP IO Int a
systemSP cmd = getHSP $ \id ->
    waitSP $ do
        let tf = "tmp_"++show id
        atom $ print ("go go go! ",tf)
        atom $ system ("rm -f "++tf++"; ("++cmd++"; touch "++tf++") &")
        wait $ doesFileExist tf
        atom $ print "really done"
        atom $ return nullSP

runSP' (ActionSP m) xs  wq = io m >>= \r -> runSP'' r xs wq
runSP' (GetSP f) (x:xs) wq = runSP'' (f (Just x)) xs wq
runSP' (GetSP f)  []    [] = runSP'' (f Nothing) [] []
runSP' (GetSP f)  []    wq = runSP'' (GetSP f) [] wq
runSP' NullSP     _     wq = return []
runSP' (PutSP (High o) sp') xs wq = runSP'' sp' xs wq >>= \os -> return (o:os)
runSP' (PutSP (Low w) sp') xs wq = runSP'' sp' xs (w:wq)

runSP'' sp' xs [] = runSP' sp' xs []
runSP'' sp' xs ((p,w):wq) = case w of
     ReqId -> do
            i <- getst
            setst (succ i)
            runSP' sp' (Low (p,NextId i):xs) wq
     Wait donefn -> do
        b <- io donefn
        if b then runSP' sp' (Low (p,Done):xs) wq
         else runSP' sp' xs ((p,w):wq)


test = runstp (runSP' sp' [] []) 0
 where sp = systemSP "sleep 2" <=< getIdSP
       sp' = sp ||+|| sp

