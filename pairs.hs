{-# LANGUAGE ImpredicativeTypes #-}
module ChurchPairs where
import Prelude(($))

{-Church Pairs-}
pair :: a -> b -> (a -> b -> c) -> c
pair = \a b p -> p a b

first :: ((a -> b -> a) -> c) -> c
first = \p -> p $ \a b -> a

scnd :: ((a -> b -> b) -> c) -> c
scnd = \p -> p $ \a b -> b

{-Church Pair Utilities-}

chPtoHsT = \p -> (first p, scnd p)
