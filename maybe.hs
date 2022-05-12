{-# LANGUAGE ImpredicativeTypes #-}
module ChurchMaybe where
import Prelude(($), Maybe(Nothing, Just), error)
import ChurchBools
import ChurchCombs

{-Church Maybe Type-}
type ChM a = forall r. r -> (a -> r) -> r

{-Church Maybes-}
nothing :: ChM a
nothing = \n j -> n

just :: a -> ChM a
just = \x n j -> j x

{-Church Maybe Utilities-}
isnothing :: ChM a -> ChB
isnothing = \m -> m false (const true)

isjust :: ChM a -> ChB
isjust = \m -> m true (const false)

fromjust :: ChM a -> a
fromjust = \m -> m (error "") id

chMtoHsM :: ChM a -> Maybe a
chMtoHsM = \m -> isnothing m Nothing $ Just $ fromjust m
