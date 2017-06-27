
module Misc(
    retr, ifM, ifM_, fix, fst3, snd3, trd3, testsize,
    fst4, snd4, trd4, fth4,
    maybeHead, mapfst, mapsnd, catchMaybe, mk_ival, fill_ival, tpmap,
    filterJust, gen_map, map2, map3, map4, map5, mapf, mapf4, mapf5, mapf3, mapf2,
--    observe, Observable, send, (<<), observer, observeOpaque, runO,
    trace, dbg, compMap, maybeM, maybeM_, norm_rnd, norm_rnds, collectM, repeatM,
    (.!), ($^),
    around, iterate',
    isLeft, isRight
    ) where

import Monad
import Maybe
import List
import Random

--import Observe
import IOExts
--import IOExtras


isLeft (Left _) = True
isLeft _ = False
isRight (Right _) = True
isRight _ = False

retr :: Eq a => a -> [(a,b)] -> (Maybe b, [(a,b)])
retr key list = retr' list []
 where
  retr' [] vs = (Nothing, vs)
  retr' ((a,b):xs) vs
   | key == a  = (Just b, vs++xs)
   | otherwise = retr' xs ((a,b):vs)

ifM :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
ifM Nothing _ = return Nothing
ifM (Just a) f = Just $^ f a

ifM_ m f = ifM m f >> return ()

fix :: (a -> a) -> a
fix f = let x = f x in x

fst3 :: (a,b,c) -> a
snd3 :: (a,b,c) -> b
trd3 :: (a,b,c) -> c

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

fst4 (a,_,_,_) = a
snd4 (_,b,_,_) = b
trd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

testsize :: [a] -> ([a] -> b) -> b -> b
testsize [] _ e = e
testsize xs f _ = f xs

maybeHead :: a -> [a] -> a
maybeHead o [] = o
maybeHead _ (x:_) = x

mapfst :: (a -> b) -> [(a,c)] -> [(b,c)]
mapsnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapfst f = map (\(a,b) -> (f a, b))
mapsnd f = map (\(a,b) -> (a, f b))

tpmap f (a,b) = (f a, f b)

catchMaybe :: IO a -> IO (Maybe a)
catchMaybe m = liftM Just m `catch` (\_ -> return Nothing)

filterJust :: [Maybe a] -> [a]
filterJust = map fromJust . filter isJust

gen_map :: ([a] -> (b, [a])) -> [a] -> [b]
gen_map _ [] = []
gen_map f xs = let (a',b) = f xs in a' : gen_map f b

-- Shouldn't these be in the Prelude?

map2::(a->b->c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = (f x y):(map2 f xs ys)
map2 _ _ _ = []

map3::(a->b->c->d) -> [a] -> [b] -> [c] -> [d]
map3 f (x:xs) (y:ys) (z:zs) = (f x y z):(map3 f xs ys zs)
map3 _ _ _ _ = []

map4::(a->b->c->d->e) -> [a] -> [b] -> [c] -> [d] -> [e]
map4 f (x:xs) (y:ys) (z:zs) (w:ws) = (f x y z w):(map4 f xs ys zs ws)
map4 _ _ _ _ _ = []

map5::(a->b->c->d->e->f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
map5 f (x:xs) (y:ys) (z:zs) (w:ws) (q:qs) = (f x y z w q):(map5 f xs ys zs ws qs)
map5 _ _ _ _ _ _ = []

mapf::[(a->b)] -> [a] -> [b]
mapf (f:fs) (x:xs) = (f x):(mapf fs xs)
mapf _ _ = []

mapf5::[(a->b->c->d->e->f)] -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
mapf5 (f:fs) (a:as) (b:bs) (c:cs) (d:ds) (e:es)  = (f a b c d e):(mapf5 fs as bs cs ds es)
mapf5 _ _ _ _ _ _ = []

mapf4::[(a->b->c->d->e)] -> [a] -> [b] -> [c] -> [d] -> [e]
mapf4 (f:fs) (a:as) (b:bs) (c:cs) (d:ds) = (f a b c d):(mapf4 fs as bs cs ds)
mapf4 _ _ _ _ _ = []

mapf3::[(a->b->c->d)] -> [a] -> [b] -> [c] -> [d]
mapf3 (f:fs) (a:as) (b:bs) (c:cs) = (f a b c):(mapf3 fs as bs cs)
mapf3 _ _ _ _ = []

mapf2::[(a->b->c)] -> [a] -> [b] -> [c]
mapf2 (f:fs) (a:as) (b:bs) = (f a b):(mapf2 fs as bs)
mapf2 _ _ _ = []

dbg a = trace (show a)
--dbg _ = id

compMap :: (a -> b -> b) -> [a] -> b -> b
compMap _ [] = id
compMap f (x:xs) = f x . compMap f xs

maybeM _ Nothing = return Nothing
maybeM m (Just a) = liftM Just $ m a

maybeM_ _ Nothing = return ()
maybeM_ m (Just a) = m a >> return ()

norm_rnd :: RandomGen r => r -> (Float,r)
norm_rnd r = randomR (0,1) r

norm_rnds :: RandomGen r => r -> ([Float], r)
norm_rnds r = let (r1,r2) = split r in (randoms r1, r2)

collectM n m i = repeatM n (m i)

repeatM n m = mapM (const m) [1..n]

mk_ival :: (Ord a, Num a) => a -> a -> a -> [a]
mk_ival start step stop
 | start > stop = [stop]
 | otherwise    =
    start : case mk_ival (start+step) step stop of
             [x,y] -> if x == y then [x] else [x,y]
             xs -> xs


{-
    fills in extra values in a list to make sure the that the
    difference between consecutive values is at most "step"
-}

fill_ival :: (Ord a, Fractional a) => [a] -> a -> [a]
fill_ival [] _ = []
fill_ival [x] _ = [x]
fill_ival (x:y:xs) step
 | y - x > step = let nxt = x + min ((y-x)/2) step
                  in x : fill_ival (nxt:y:xs) step
 | otherwise    = x : fill_ival (y:xs) step


-- Gah, vill använda #, men det går inte tillsammans med -fglasgow-exts
infixl 9 .!

(.!) :: s -> (s -> f) -> f
s .! f = f s

infixr 0 $^
-- Same as $, but lifts (^) the function  ;)
($^) :: Monad m => (a -> b) -> m a -> m b
f $^ m = liftM f m

-- ($^^) :: 

around i f m = do
    s <- i
    r <- m
    f s
    return r


iterate' f i = i : seq i' (iterate' f i')
 where i' = f i

