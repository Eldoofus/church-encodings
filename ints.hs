{-# LANGUAGE RankNTypes #-}
module ChurchInts where
import Prelude(Integer, IO, ($), print, (-), map, (<), (!!))
import Data.Typeable
import ChurchBools
import ChurchCombs
import ChurchNats
import ChurchPairs

{-Church Integer Type-}
type ChZ = ChP ChN ChN
newtype ChI = I { unI :: ChZ }

{-Church Integers-}
zeroI :: ChI
zeroI = I $ pair zero zero

{-Church Integral Operators-}
succI :: ChI -> ChI
succI = \i -> I $ pair (succ $ first $ unI i) (scnd $ unI i)

plusI :: ChI -> ChI -> ChI
plusI = \i j -> I $ pair (plus (first $ unI i) (first $ unI j)) (plus (scnd $ unI i) (scnd $ unI j))

predI :: ChI -> ChI
predI = \i -> I $ pair (first $ unI i) (succ $ scnd $ unI i)

minusI :: ChI -> ChI -> ChI
minusI = \i j -> I $ pair (plus (first $ unI i) (scnd $ unI j)) (plus (scnd $ unI i) (first $ unI j))

multI :: ChI -> ChI -> ChI
multI = \i j -> I $ (\a b c d -> pair (plus (mult a c) (mult b d)) (plus (mult a d) (mult b c))) (first $ unI i) (scnd $ unI i) (first $ unI j) (scnd $ unI j)

divZ :: ChN -> ChN -> ChN
divZ = \n m -> iszero m zero $ div n m

divI :: ChI -> ChI -> ChI
divI = \i j -> I $ (\a b c d -> pair (plus (divZ a c) (divZ b d)) (plus (divZ a d) (divZ b c))) (first $ unI $ oneZ i) (scnd $ unI $ oneZ i) (first $ unI $ oneZ j) (scnd $ unI $ oneZ j)

modI :: ChI -> ChI -> ChI
modI = \i j -> minusI i $ multI j $ divI i j

gcdI :: ChI -> ChI -> ChI
gcdI = y $ \c i j -> equlI i zeroI j $ c (modI j i) i

neg :: ChI -> ChI
neg = \i -> I $ pair (scnd $ unI i) (first $ unI i)

{-Church Integral Untilities-}
chNtoChI :: ChN -> ChI
chNtoChI = \n -> I $ pair n zero

chItoHsI :: ChI -> Integer
chItoHsI = \i -> chNtoHsI (first $ unI i) - chNtoHsI (scnd $ unI i)

printChI :: ChI -> IO ()
printChI = \i -> print $ chItoHsI i

oneZ :: ChI -> ChI
oneZ = \i -> I $ (greq (first $ unI i) (scnd $ unI i)) (\a b p -> p a b) (\a b p -> p b a) (minus (first $ unI i) (scnd $ unI i)) zero

chI :: Integer -> ChI
chI i = if i < 0 then neg (chI $ -i) else chNtoChI $ chN i

{-Church Integral Functions-}

abs :: ChI -> ChI
abs = \i -> I $ pair (plus (first $ unI $ oneZ i) (scnd $ unI $ oneZ i)) zero

{-Church Integral Predicates-}
iszeroI :: ChI -> ChB
iszeroI = \i -> equl (first $ unI i) (scnd $ unI i)

isNeg :: ChI -> ChB
isNeg = \i -> less (first $ unI i) (scnd $ unI i)

{-Church Integral Comparators-}
greqI :: ChI -> ChI -> ChB
greqI = \i j -> not $ isNeg $ minusI i j

lseqI :: ChI -> ChI -> ChB
lseqI = \i j -> not $ isNeg $ minusI j i

lessI :: ChI -> ChI -> ChB
lessI = \i j -> isNeg $ minusI i j

grtrI :: ChI -> ChI -> ChB
grtrI = \i j -> isNeg $ minusI j i

equlI :: ChI -> ChI -> ChB
equlI = \i j -> iszeroI $ minusI i j

neqlI :: ChI -> ChI -> ChB
neqlI = \i j -> not $ iszeroI $ minusI i j