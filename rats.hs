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
plusR = \r s -> r

{-Church Rational Utilities-}
red :: ChR -> ChR
red = \r -> pair (divI (first r) $ gcdI (abs $ first r) (abs $ scnd r)) (divI (scnd r) (gcdI (abs $ first r) (abs $ scnd r)))

chRtoHsD :: ChR -> Double
chRtoHsD = \r -> (fromIntegral $ chItoHsI $ first r) / (fromIntegral $ chItoHsI $ scnd r)

printChR :: ChR -> IO ()
printChR = \r -> print $ chRtoHsD r

{-Church Rational Predicates-}

{-Church Rational Comparators-}
