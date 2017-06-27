
module BSP where


import SP
import SPHelper


data Bi a b = Low a | High b
 deriving Show
data Sel = L | R
 deriving Show

type BSP m i o ri ro = SP m (Bi ([Sel], ri) i) (Bi ([Sel], ro) o)

(||+||) :: Monad m => BSP m il ol ri ro -> BSP m ir or ri ro -> BSP m (Either il ir) (Either ol or) ri ro
sp1 ||+|| sp2 = mapSP post <-< (sp1 |+| sp2) <-< mapSP pre
 where pre msg = case msg of
                  High (Left h) -> Left (High h)
                  High (Right h) -> Right (High h)
                  Low (L:path, r) -> Left (Low (path,r))
                  Low (R:path, r) -> Right (Low (path,r))
       post msg = case msg of
                   Left (High h) -> High (Left h)
                   Right (High h) -> High (Right h)
                   Left (Low (path,r)) -> Low (L:path,r)
                   Right (Low (path,r)) -> Low (R:path,r)


(<=<) :: Monad m => BSP m t o ri ro -> BSP m i t ri ro -> BSP m i o ri ro
sp1 <=< sp2 = loopLeftSP (mapSP post <-< (sp1 ||+|| sp2) <-< mapSP pre)
 where pre msg = case msg of
                  Right (High h) -> High (Right h)
                  Right (Low l) -> Low l
                  Left (High h) -> High (Left h)
       post msg = case msg of
                   High (Right r) -> Left (High r)
                   High (Left l) -> Right (High l)
                   Low l -> Right (Low l)


mapBSP f = getSP $ \msg -> 
    case msg of Low _ -> mapBSP f
                High h -> putSP (High (f h)) (mapBSP f)

ping :: Int -> BSP IO (Maybe Int) (Maybe Int) Int Int
ping n = putSP (Low ([],n)) ping'
 where ping' = getSP $ \i -> case i of
                              Low (_,l) -> putSP (High $ if l == n then (Just n) else Nothing) ping'
                              High h -> putSP (High h) ping'

bspSP sp = loopThroughRightSP route sp
 where route = getSP $ \i -> actionSP $ 
        case i of
         Left (High h) -> return $ putSP (Right h) route
         Right r -> return $ putSP (Left (High r)) route
         Left (Low (p,l)) -> (print l >> return (putSP (Left (Low (p,l `mod` 4))) route))


