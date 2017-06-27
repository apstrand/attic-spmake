
module SP(SP(..), putSP, getSP, getRSP, nullSP, actionSP, runSP,
	  (<-<), (|*|), putOneThen, afterSP, loopLeftSP) where

infixr 4 <-<
infixl 5 |*|


data SP m a b
     = PutSP b (SP m a b)
     | GetSP (Maybe a -> SP m a b)
     | NullSP
     | ActionSP (m (SP m a b))

putSP    = PutSP
getRSP   = GetSP
nullSP   = NullSP
actionSP = ActionSP

getSP f = getRSP (\a -> maybe nullSP f a)

-- Serial composition, aborts when the first SP is null
(<-<) :: Monad m => SP m b c -> SP m a b -> SP m a c
ActionSP m  <-< sp2         = actionSP ( m >>= \r -> return ( r <-< sp2 ))
PutSP o sp1 <-< sp2         = PutSP o (sp1 <-< sp2)
GetSP f     <-< NullSP      = f Nothing <-< NullSP
GetSP f     <-< PutSP o sp2 = f (Just o) <-< sp2
sp1         <-< GetSP f2    = GetSP (\i -> sp1 <-< f2 i)
sp1         <-< ActionSP m  = actionSP ( m >>= \r -> return ( sp1 <-< r ))
NullSP      <-< sp2         = NullSP


-- Parallel composition, note that it alternates its arguments every put
-- (not really needed anymore, but doesn't hurt)
(|*|) :: Monad m => SP m a b -> SP m a b -> SP m a b
ActionSP m  |*| sp2         = actionSP ( m >>= \r -> return ( r |*| sp2 ))
PutSP o sp1 |*| sp2         = PutSP o (sp2 |*| sp1)
sp1         |*| PutSP o sp2 = PutSP o (sp2 |*| sp1)
GetSP f1    |*| GetSP f2    = GetSP (\i -> f1 i |*| f2 i)
sp1         |*| ActionSP m  = actionSP ( m >>= \r -> return ( sp1 |*| r ))
NullSP      |*| sp2         = sp2
sp1         |*| NullSP      = sp1


-- when the second SP is null, become the first SP
afterSP :: Monad m => SP m a b -> SP m a b -> SP m a b
sp `afterSP` NullSP	 = sp
sp `afterSP` ActionSP m	 = actionSP (m >>= \r -> return (sp `afterSP` r))
sp `afterSP` PutSP o sp' = putSP o (sp `afterSP` sp')
sp `afterSP` GetSP f2	 = getRSP (\i -> sp `afterSP` (f2 i))

-- run the first SP one step (one put), then become the second SP
putOneThen :: Monad m => SP m a b -> SP m a b -> SP m a b
putOneThen NullSP sp2 	    = sp2
putOneThen (GetSP _) sp2    = sp2
putOneThen (PutSP o _) sp2  = putSP o sp2
putOneThen (ActionSP m) sp2 = actionSP (m >>= \r -> return (putOneThen r sp2))


loopLeftSP :: Monad m => SP m (Either l i) (Either l o) -> SP m i o
loopLeftSP = loopLeftSP' emptyq
 where loopLeftSP' q (PutSP (Left l) sp) = loopLeftSP' (pushq l q) sp
       loopLeftSP' q (PutSP (Right r) sp) = putSP r (loopLeftSP' q sp)
       loopLeftSP' q (ActionSP m) = actionSP (m >>= \r -> return (loopLeftSP' q r))
       loopLeftSP' q@([],[]) (GetSP f) = getSP (\i -> loopLeftSP' q (f (Just (Right i))))
       loopLeftSP' q (GetSP f) = let (x,q') = popq q
                                 in loopLeftSP' q' (f (Just (Left x)))
       loopLeftSP' q NullSP = nullSP

runSP :: Monad m => SP m a b -> [a] -> m [b]
runSP (ActionSP m) xs 	= m >>= \r -> runSP r xs
runSP (PutSP o sp') xs 	= runSP sp' xs >>= \os -> return (o:os)
runSP (GetSP f) (x:xs) 	= runSP (f (Just x)) xs
runSP (GetSP f) [] 	= runSP (f Nothing) []
runSP NullSP _ 		= return []


emptyq = ([],[])
pushq a (xs,ys) = (xs,a:ys)
popq (x:xs,ys) = (x,(xs,ys))
popq ([],ys) = popq (reverse ys, [])

