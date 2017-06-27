
module SPHelper where

import SP

instance Monad m => Functor (SP m a) where
	fmap f sp = mapSP f <-< sp


liftMSP :: Monad m => (a -> m b) -> SP m a b
liftMSP m = getSP (\a -> actionSP (m a >>=
		    \b -> return (putSP b (liftMSP m))))

doSP :: Monad m => (m b) -> SP m a b
doSP m = actionSP (m >>= \o -> return (putSP o (doSP m)))

doThenSP :: Monad m => (m b) -> SP m a b -> SP m a b
doThenSP m sp = actionSP (m >>= \o -> return (putSP o sp))

insSP :: Monad m => SP m a b -> SP m b b
insSP sp1 = (sp1 <-< nullSP) |*| idSP

mapSP :: Monad m => (a -> b) -> SP m a b
mapSP f = getSP $ \x -> putSP (f x) (mapSP f)

concatMapSP :: Monad m => (a -> [b]) -> SP m a b
concatMapSP f = getSP $ \x -> putList (f x) (concatMapSP f)

filterSP :: (a -> Bool) -> SP m a a
filterSP p = getSP $ \x -> if p x then putSP x (filterSP p)
				  else filterSP p

idSP = getSP (\i -> putSP i idSP)

putList ls sp = foldr putSP sp ls

put1SP a = putSP a nullSP

{-
putList [] sp = sp
putList (x:xs) sp = putSP x (putList xs sp)
-}

--put1 sp = getSP (\i -> putSP i sp)

toBothSP :: Monad m => SP m i (Either i i)
toBothSP = concatMapSP (\x -> [Left x, Right x])


filterLeftSP = filterSP (either (\_->True) (\_->False))
filterRightSP = filterSP (either (\_->False) (\_->True))

sp1 |+| sp2 = (mapSP Left <-< sp1 <-< mapSP (\(Left l) -> l) <-< filterLeftSP)
          |*| (mapSP Right <-< sp2 <-< mapSP (\(Right r) -> r) <-< filterRightSP)

fromEither = either id id

loopSP sp = loopLeftSP (toBothSP <-< sp <-< mapSP fromEither)

loopThroughRightSP :: Monad m => SP m (Either oldo i) (Either oldi o) -> SP m oldi oldo -> SP m i o
loopThroughRightSP newsp oldsp = loopLeftSP (mapSP post <-< (oldsp |+| newsp) <-< mapSP pre)
 where pre (Right i) = Right (Right i)
       pre (Left (Left i)) = Left i
       pre (Left (Right i)) = Right (Left i)
       post (Right (Right i)) = Right i
       post (Right (Left i)) = Left (Left i)
       post (Left i) = Left (Right i)

parSP sp xs = foldr (|*|) nullSP (map sp xs)

(|^|) a b = mapSP fromEither <-< (a |+| b)

