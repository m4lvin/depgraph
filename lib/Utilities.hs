module Utilities where

import qualified Data.Map.Strict as Map
import Data.Maybe

-- combinators
(-:) :: a -> (a -> b) -> b
x -: f = f x

doIf :: Bool -> (a -> a) -> (a -> a)
doIf p f x = if p then f x else x

ifelselist :: [(Bool, a)] -> a -> a
ifelselist li y =
  case li of
    [] -> y
    ((b, x) : li2) -> if b then x else ifelselist li2 y

iflist :: [(Bool, a)] -> a
iflist [] = error "iflist failed: empty list"
iflist ((b, x) : li2) = if b then x else iflist li2

loopUntilFail :: (a -> Maybe a) -> a -> a
loopUntilFail f x =
  case f x of
    Just y -> loopUntilFail f y
    Nothing -> x

loopUntil :: (a -> Bool) -> (a -> a) -> a -> a
loopUntil p f x =
  if p x then x else loopUntil p f (f x)

stopBefore :: (a -> Bool) -> (a -> a) -> a -> a
stopBefore p f x =
  if p (f x) then x else stopBefore p f (f x)

removeJustWithDefault :: Maybe a -> a -> a
removeJustWithDefault x y = fromMaybe y x

justRight :: Either a b -> b
justRight (Right x) = x
justRight (Left _) = error "justRight was given a Left"

lookup2 :: (Ord a) => a -> Map.Map a b -> b
lookup2 x h = fromJust (Map.lookup x h)

lookupWithDefault :: (Ord a) => a -> Map.Map a b -> b -> b
lookupWithDefault x h = removeJustWithDefault (Map.lookup x h)

-- Maps and Lists

insertMultiple :: (Ord a) => [(a, b)] -> Map.Map a b -> Map.Map a b
insertMultiple li h = foldl (\hm (x, y) -> Map.insert x y hm) h li

listUpdateFun :: Int -> (a -> a) -> [a] -> [a]
listUpdateFun n f li = listUpdate n (f (li !! n)) li

listUpdatesFun :: (Int -> Bool) -> (a -> a) -> [a] -> [a]
listUpdatesFun p f = zipWith (\i x -> doIf (p i) f x) [1 ..]

replaceSublist :: Int -> Int -> [a] -> [a] -> [a]
replaceSublist m n li li2 =
  let front = take m li2
      back = drop (max m n) li2
   in front ++ li ++ back

listUpdate :: Int -> a -> [a] -> [a]
listUpdate n x = replaceSublist n (n + 1) [x]

filterZip :: (b -> Bool) -> [a] -> [b] -> [(a, b)]
filterZip p as bs = filter (\(_, y) -> p y) (zip as bs)

cofilter :: (b -> Bool) -> [a] -> [b] -> ([a], [b])
cofilter p as bs = unzip (filterZip p as bs)

lookupList :: (Ord a) => [a] -> Map.Map a b -> [Maybe b]
lookupList as mp = fmap (`Map.lookup` mp) as

lookupList2 :: (Ord a) => [a] -> Map.Map a b -> [b]
lookupList2 as mp = fmap (`lookup2` mp) as

inj2 :: a -> b -> (a, b)
inj2 x y = (x, y)

injf2 :: (b -> c) -> (a, b) -> (a, c)
injf2 f (x, y) = (x, f y)

inj1 :: b -> a -> (a, b)
inj1 y x = (x, y)

injf1 :: (a -> c) -> (a, b) -> (c, b)
injf1 f (x, y) = (f x, y)

mlookup :: Int -> [a] -> Maybe a
mlookup n li = if 0 <= n && n < length li then Just (li !! n) else Nothing

(*>) :: (a -> b) -> (b -> c) -> (a -> c)
f *> g = g . f

tryWithDefault :: (a -> Maybe b) -> b -> a -> b
tryWithDefault f def x =
  fromMaybe def (f x)

foldIterate :: (a -> c -> c) -> [a] -> c -> c
foldIterate f as x = foldl (flip f) x as

foldIterate2 :: (a -> b -> c -> c) -> [a] -> [b] -> c -> c
foldIterate2 f as bs x = foldl (\y (xa, xb) -> f xa xb y) x (zip as bs)

-- for :: a -> (a -> a) -> (a -> Bool) -> (a -> a) -> a

sublist :: Int -> Int -> [a] -> [a]
sublist m n = take (n - m) . drop m

-- >=m, <n

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 = zipWith

infixr 0 <|

(<|) :: (a -> b) -> a -> b
(<|) = ($)

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
