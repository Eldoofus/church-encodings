{-# LANGUAGE ImpredicativeTypes #-}
module ChurchInts where
import Prelude(Integer, IO, ($), print, (-), map, (<), (!!))
import ChurchBools
import ChurchCombs
import ChurchNats
import ChurchPairs

{-Church Integer Type-}
type ChI = ChP ChN ChN

{-Church Integers-}
zeroI :: ChI
zeroI = pair zero zero

{-Church Integral Operators-}
succI :: ChI -> ChI
succI = \i -> pair (succ $ first i) (scnd i)

plusI :: ChI -> ChI -> ChI
plusI = \i j -> pair (plus (first i) (first j)) (plus (scnd i) (scnd j))

predI :: ChI -> ChI
predI = \i -> pair (first i) (succ $ scnd i)

minusI :: ChI -> ChI -> ChI
minusI = \i j -> pair (plus (first i) (scnd j)) (plus (scnd i) (first j))

multI :: ChI -> ChI -> ChI
multI = \i j -> pair (plus (mult (first i) (first j)) (mult (scnd i) (scnd j))) (plus (mult (first i) (scnd j)) (mult (scnd i) (first j)))

divZ :: ChN -> ChN -> ChN
divZ = \n m -> iszero m zero $ div n m

divI :: ChI -> ChI -> ChI
divI = \i j -> pair (plus (divZ (first $ oneZ i) (first $ oneZ j)) (divZ (scnd $ oneZ i) (scnd $ oneZ j))) (plus (divZ (first $ oneZ i) (scnd $ oneZ j)) (divZ (scnd $ oneZ i) (first $ oneZ j)))

modI :: ChI -> ChI -> ChI
modI = \i j -> minusI i $ multI j $ divI i j

gcdI :: ChI -> ChI -> ChI
gcdI = y $ \c i j -> equlI i zeroI j $ c (modI j i) i

{-Church Integral Untilities-}
chNtoChI :: ChN -> ChI
chNtoChI = \n -> pair n zero

chItoHsI :: ChI -> Integer
chItoHsI = \i -> chNtoHsI (first i) - chNtoHsI (scnd i)

printChI :: ChI -> IO ()
printChI = \i -> print $ chItoHsI i

oneZ :: ChI -> ChI
oneZ = \i -> pair ((greq (first i) (scnd i)) (minus (first i) (scnd i)) zero) ((greq (first i) (scnd i)) zero (minus (scnd i) (first i)))

chI :: Integer -> ChI
chI i = if i < 0 then neg (chI $ -i) else chNtoChI $ chN i

{-Church Integral Functions-}

abs :: ChI -> ChI
abs = \i -> pair (plus (first $ oneZ i) (scnd $ oneZ i)) zero

neg :: ChI -> ChI
neg = \i -> pair (scnd i) (first i)

{-Church Integral Predicates-}
iszeroI :: ChI -> ChB
iszeroI = \i -> equl (first i) (scnd i)

isneg :: ChI -> ChB
isneg = \i -> less (first i) (scnd i)

{-Church Integral Comparators-}
greqI :: ChI -> ChI -> ChB
greqI = \i j -> not $ isneg $ minusI i j

lseqI :: ChI -> ChI -> ChB
lseqI = \i j -> not $ isneg $ minusI j i

lessI :: ChI -> ChI -> ChB
lessI = \i j -> isneg $ minusI i j

grtrI :: ChI -> ChI -> ChB
grtrI = \i j -> isneg $ minusI j i

equlI :: ChI -> ChI -> ChB
equlI = \i j -> iszeroI $ minusI i j

neqlI :: ChI -> ChI -> ChB
neqlI = \i j -> not $ iszeroI $ minusI i j
