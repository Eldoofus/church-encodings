{-# LANGUAGE ImpredicativeTypes,ScopedTypeVariables #-}
module ChurchLists where
import Prelude(($), error, IO, print, Show)
import ChurchBools
import ChurchCombs

{-Church List Type-}
type ChL a = forall r. (a -> r -> r) -> r -> r

{-Church Lists-}
nil :: ChL a
nil = \c n -> n

cons :: a -> ChL a -> ChL a
cons = \h t c n -> c h $ t c n

{-Church List Utilities-}
isnil :: ChL a -> ChB
isnil = \l -> l (\h t -> false) true

head :: ChL a -> a
head = \l -> l const (error "")

tail :: ChL a -> ChL a
tail = \l c n -> l (\h t g -> g h $ t c) (const n) (const id)

chLtoHsL :: ChL a -> [a]
chLtoHsL = \l -> l (:) []
