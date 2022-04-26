{-# LANGUAGE RankNTypes #-}
module ChurchBools where
import Prelude(Bool(True, False), IO, ($), print)

{-Church Boolean Type-}
type ChB = forall a. a -> a -> a

{-Church Booleans-}
true :: ChB
true = \t f -> t

false :: ChB
false = \t f -> f

{-Church Boolean Utilities-}
chBtoHsB :: ChB -> Bool
chBtoHsB = \b -> b True False

printChB :: ChB -> IO ()
printChB = \b -> print $ chBtoHsB b

{-Church Boolean Operators-}
not :: ChB -> ChB
not = \b -> b false true

and :: ChB -> ChB -> ChB
and = \a b -> a b false

or :: ChB -> ChB -> ChB
or = \a b -> a true b

xor :: ChB -> ChB -> ChB
xor = \a b -> a (not b) b