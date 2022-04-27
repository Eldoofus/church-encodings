{-# LANGUAGE ImpredicativeTypes, TypeApplications #-}
module ChurchRats where
import Prelude(fromIntegral, Integer, Double, IO, (/), ($), print)
import ChurchBools
import ChurchCombs
import ChurchNats
import ChurchPairs
import ChurchInts

{-Church Rational Type-}
type ChR = (ChI -> ChI -> ChI) -> ChI

{-Church Rationals-}
zeroR :: ChR
zeroR = pair zeroI zeroI

{-Church Rational Operators-}
plusR :: ChR -> ChR -> ChR
plusR = \q r -> red $ pair (plusI (multI (first q) (scnd r)) (multI (scnd q) (first r))) $ multI (scnd q) (scnd r)

minusR :: ChR -> ChR -> ChR
minusR = \q r -> red $ pair (minusI (multI (first q) (scnd r)) (multI (scnd q) (first r))) $ multI (scnd q) (scnd r)

multR :: ChR -> ChR -> ChR
multR = \q r -> red $ pair (multI (first q) (first r)) (multI (scnd q) (scnd r))

divR :: ChR -> ChR -> ChR
divR = \q r -> red $ pair (multI (first q) (scnd r)) (multI (scnd q) (first r))

{-Church Rational Utilities-}
red :: ChR -> ChR
red = \r -> pair (divI (first r) $ gcdI (abs $ first r) (abs $ scnd r)) (divI (scnd r) (gcdI (abs $ first r) (abs $ scnd r)))

chNtoChR :: ChN -> ChR
chNtoChR = \n -> chItoChR $ chNtoChI n

chItoChR :: ChI -> ChR
chItoChR = \i -> pair i $ succI zeroI

chRtoHsD :: ChR -> Double
chRtoHsD = \r -> (fromIntegral $ chItoHsI $ first r) / (fromIntegral $ chItoHsI $ scnd r)

printChR :: ChR -> IO ()
printChR = \r -> print $ chRtoHsD r

chR :: Integer -> Integer -> ChR
chR = \i j -> pair (chI i) (chI j)

{-Church Rational Functions-}
absR :: ChR -> ChR
absR = \r -> pair (abs $ first r) (abs $ scnd r)

negR :: ChR -> ChR
negR = \r -> pair (neg $ first r) $ scnd r

floor :: ChR -> ChR
floor = \r -> pair (divI (first r) (scnd r)) $ succI zeroI

ceil :: ChR -> ChR
ceil = \r -> pair (plusI (isint zeroI $ succI zeroI) $ divI (first r) (scnd r)) $ succI zeroI

round :: ChR -> ChR
round = \r -> floor $ plusR r $ pair (succI zeroI) $ succI $ succI zeroI

{-Church Rational Predicates-}
iszeroR :: ChR -> ChB
iszeroR = \r -> iszeroI $ first r

isnegR :: ChR -> ChB
isnegR = \r -> xor (isneg $ first r) (isneg $ scnd r)

isint :: ChR -> ChB
isint = \r -> iszeroI $ predI $ scnd $ red r

isinf :: ChR -> ChB
isinf = \r -> iszeroI $ scnd r

{-Church Rational Comparators-}
greqR :: ChR -> ChR -> ChB
greqR = \q r -> not $ isnegR $ minusR q r

lseqR :: ChR -> ChR -> ChB
lseqR = \q r -> not $ isnegR $ minusR r q

lessR :: ChR -> ChR -> ChB
lessR = \q r -> isnegR $ minusR q r

grtrR :: ChR -> ChR -> ChB
grtrR = \q r -> isnegR $ minusR r q

equlR :: ChR -> ChR -> ChB
equlR = \q r -> iszeroR $ minusR q r

neqlR :: ChR -> ChR -> ChB
neqlR = \q r -> not $ iszeroR $ minusR q r