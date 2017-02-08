module Lib where

import Data.List

type AList k v = [(k, v)]

alist_lookup :: Ord k => k -> AList k v -> Maybe v
alist_lookup _          [] = Nothing
alist_lookup a ((k, v):as) =
  if k == a then
    Just v
  else
    alist_lookup a as

opt_bind :: Maybe a -> b -> AList a b -> AList a b
opt_bind n v e =
  case n of
    Nothing -> e
    Just n' -> (n', v):e

allDistinct :: (Ord a, Eq a) => [a] -> Bool
allDistinct xs = checkSortedList (sort xs)
  where
    checkSortedList []       = True
    checkSortedList [x]      = True
    checkSortedList (x:y:ys) = x /= y && checkSortedList (y:ys)
