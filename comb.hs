{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module ChurchCombs where
import Prelude(($), (.))

{-Church Combinators-}
newtype Mu a = Mu (Mu a -> a)
y = \f -> (\h -> h $ Mu h) $ \x -> f . (\(Mu g) -> g) x $ x

id :: a -> a
id = \x -> x

const :: a -> b -> a
const = \k -> \x -> k