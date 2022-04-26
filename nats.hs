{-# LANGUAGE ImpredicativeTypes #-}
module ChurchNats where
import Prelude(Integer, IO, ($), print, (+), (-), (!!))
import ChurchBools
import ChurchCombs

{-Church Natural Type-}
type ChN = forall a. (a -> a) -> a -> a

{-Church Naturals-}
zero :: ChN
zero = \f x -> x

{-Church Natural Operators-}
succ :: ChN -> ChN
succ = \n f x -> f $ n f x

plus :: ChN -> ChN -> ChN
plus = \n m f x -> m f $ n f x

mult :: ChN -> ChN -> ChN
mult = \n m x -> n $ m x

exp :: ChN -> ChN -> ChN
exp = \n m -> m n

pred :: ChN -> ChN
pred = \n f x -> n (\g h -> h $ g f) (const x) id

minus :: ChN -> ChN -> ChN
minus = \n m -> m pred n

div :: ChN -> ChN -> ChN
div = y $ \c n m -> (greq n m) (succ $ c (minus n m) m) zero

{-Church Natural Utilities-}
chNtoHsI :: ChN -> Integer
chNtoHsI = \n -> n (\x -> x + 1) 0

printChN :: ChN -> IO ()
printChN = \n -> print $ chNtoHsI n

chN :: Integer -> ChN
chN 0 = \f x -> x
chN n = \f x -> f $ (chN $ n - 1) f x

{-Church Natural Predicates-}
iszero :: ChN -> ChB
iszero = \n -> n (const false) true

{-Church Natural Comparators-}
lseq :: ChN -> ChN -> ChB
lseq = \n m -> iszero $ minus n m

greq :: ChN -> ChN -> ChB
greq = \n m -> iszero $ minus m n

less :: ChN -> ChN -> ChB
less = \n m -> iszero (minus m n) false true

grtr :: ChN -> ChN -> ChB
grtr = \n m -> iszero (minus n m) false true

equl :: ChN -> ChN -> ChB
equl = \n m -> and (iszero $ minus n m) (iszero $ minus m n)

neql :: ChN -> ChN -> ChB
neql = \n m -> not $ and (iszero $ minus n m) (iszero $ minus m n)