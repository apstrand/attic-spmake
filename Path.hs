
module Path where

import List

splitname fn =
    case break (\c -> c =='.' || c == '/' || c == '\\') (reverse fn) of
     (ext, ('.':fn')) -> (reverse fn', reverse ext)
     _ -> (fn,"")

ext_bw_ord p q
 | ord /= EQ = ord
 | otherwise = (compare a c)
 where
	(a,b) = splitname p
	(c,d) = splitname q
	ord   = compare d b


flatten_name = map (\c -> if c == '/' || c == '\\' then '_' else c)
slash_path = map (\c -> if c == '\\' then '/' else c)

chext :: String -> String -> String
chext fn ext = fst (splitname fn) ++ "." ++ ext

drop_tail p = case break (=='/') (slash_path (reverse p)) of
    ("",'/':b) -> drop_tail (reverse b)
    (_,'/':b) -> (reverse b)
    _ -> ""



merge_paths p "." = p
merge_paths "." q = q
merge_paths p "./" = p
merge_paths "./" q = q
merge_paths p [] = p
merge_paths p q@('/':_) = q
merge_paths p q@(_:':':'\\':_) = q
merge_paths "" q = q
merge_paths p ('.':'.':q) = (drop_tail p) ++ "/" ++ q
merge_paths p q = p ++ "/" ++ q

basename = fst . splitname . reverse . takeWhile (/='/') . reverse

splitpath' p = map (foldr1 (\a b -> a ++"/"++ b)) (tail $ inits (split (=='/') p))

splitpath p@('/':_) = map ("/"++) (splitpath' p)
splitpath p = splitpath' p

dirname = reverse . dropWhile (/='/') . reverse


split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
              [] -> []
              ws -> ys : (split p zs)
                     where (ys,zs) = break p ws


remove_ext = fst . splitname

