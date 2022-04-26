{-# LANGUAGE ImpredicativeTypes #-}
import Prelude(($), print, (!!))
import ChurchBools
import ChurchNats
import ChurchCombs
import ChurchPairs
import ChurchInts
import ChurchRats

main = do
    printChR $ red $ pair (chI $ -36) (chI 6)
