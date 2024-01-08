module Utilities (doIf, foldIterate, (|>), lookup2, ifelselist) where

import qualified Data.Map.Strict as Map
import Data.Maybe

doIf :: Bool -> (a -> a) -> (a -> a)
doIf p f x = if p then f x else x

ifelselist :: [(Bool, a)] -> a -> a
ifelselist li y =
  case li of
    [] -> y
    ((b, x) : li2) -> if b then x else ifelselist li2 y

lookup2 :: (Ord a) => a -> Map.Map a b -> b
lookup2 x h = fromJust (Map.lookup x h)

foldIterate :: (a -> c -> c) -> [a] -> c -> c
foldIterate f as x = foldl (flip f) x as

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x
