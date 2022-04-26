{-# LANGUAGE RankNTypes #-}
module ChurchPairs where
import Prelude(($))

{-Church Pair Type-}
type ChP a b = forall c. (a -> b -> c) -> c

{-Church Pairs-}
pair :: a -> b -> ChP a b
pair = \a b p -> p a b

first :: ChP a b -> a
first = \p -> p $ \a b -> a

scnd :: ChP a b -> b
scnd = \p -> p $ \a b -> b