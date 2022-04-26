{-# LANGUAGE ImpredicativeTypes #-}
module ChurchPairs where
import Prelude(($))

{-Church Pairs-}
pair :: a1 -> a2 -> (a1 -> a2 -> a) -> a
pair = \a b p -> p a b

first :: ((a2 -> a1 -> a2) -> a) -> a
first = \p -> p $ \a b -> a

scnd :: ((a1 -> a2 -> a2) -> a) -> a
scnd = \p -> p $ \a b -> b