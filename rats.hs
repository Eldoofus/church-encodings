{-# LANGUAGE ImpredicativeTypes #-}
module ChurchRats where
import Prelude(fromIntegral, Integer, Double, IO, (/), ($), print)
import ChurchBools
import ChurchCombs
import ChurchNats
import ChurchPairs
import ChurchInts

{-Church Rational Type-}
type ChQ = ChP ChI ChN
newtype ChR = R { unR :: ChQ }

{-Church Rationals-}
zeroR :: ChR
zeroR = R $ pair zeroI zero

{-Church Rational Operators-}
plusR :: ChR -> ChR -> ChR
plusR = \r s -> r

{-Church Rational Utilities-}
red :: ChR -> ChR
red = \r -> (\q -> R $ pair (divI (first $ unR r) q) (div (scnd $ unR r) (first $ unI $ abs q))) $ gcdI (abs $ first $ unR r) (chNtoChI $ scnd $ unR r)

chRtoHsD :: ChR -> Double
chRtoHsD = \r -> (fromIntegral $ chItoHsI $ first $ unR r) / (fromIntegral $ chNtoHsI $ scnd $ unR r)

printChR :: ChR -> IO ()
printChR = \r -> print $ chRtoHsD r

{-Church Rational Predicates-}

{-Church Rational Comparators-}
