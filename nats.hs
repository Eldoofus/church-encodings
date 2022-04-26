{-# LANGUAGE RankNTypes #-}
module ChurchNats where
import Prelude(Integer, IO, ($), print, (+), (-), (!!))
import ChurchBools
import ChurchCombs

{-Church Natural Type-}
type ChNat = forall a. (a -> a) -> a -> a
newtype ChN = N { unN :: ChNat }

{-Church Naturals-}
zero :: ChN
zero = N $ \f x -> x

{-Church Natural Operators-}
succ :: ChN -> ChN
succ = \n -> N $ \f x -> f $ unN n f x

plus :: ChN -> ChN -> ChN
plus = \n m -> N $ \f x -> unN m f $ unN n f x

mult :: ChN -> ChN -> ChN
mult = \n m -> N $ \x -> unN n $ unN m x

exp :: ChN -> ChN -> ChN
exp = \n m -> N $ unN m $ unN n

pred :: ChN -> ChN
pred = \n -> N $ \f x -> unN n (\g h -> h $ g f) (const x) id

minus :: ChN -> ChN -> ChN
minus = \n m -> unN m pred n

div :: ChN -> ChN -> ChN
div = y $ \c n m -> (greq n m) (succ $ c (minus n m) m) zero

{-Church Natural Utilities-}
chNtoHsI :: ChN -> Integer
chNtoHsI = \n -> unN n (\x -> x + 1) 0

printChN :: ChN -> IO ()
printChN = \n -> print $ chNtoHsI n

chN :: Integer -> ChN
chN 0 = N $ \f -> \x -> x
chN n = N $ \f -> \x -> f $ (unN $ chN $ n - 1) f x

{-Church Natural Predicates-}
iszero :: ChN -> ChB
iszero = \n -> unN n (const false) true

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