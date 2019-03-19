module Ch5 where

import Prelude

import Data.Array (drop)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Picture
import Math as Math

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 n = n
gcd n m
    | n > m = gcd (n - m) m
    | otherwise = gcd n (m - n)

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = fact (n - 1) + fact (n - 2)

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> ", " <> x

showPerson_ :: forall r. { first :: String, last :: String | r } -> String
showPerson_ { first: x, last: y } = y <> ", " <> x

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
    | x <= y = arr
    | otherwise = [y, x]
sortPair arr = arr

sameCity :: forall a b c d
    . { address :: { city :: String | a } | b }
   -> { address :: { city :: String | c } | d }
   -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton x _   = x

longestZeroSuffix :: Array Int -> Array Int
longestZeroSuffix [] = []
longestZeroSuffix xs = case sum xs of
    0 -> xs
    _ -> longestZeroSuffix $ drop 1 xs

originCircle :: Shape
originCircle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

doubleScale :: Shape -> Shape
doubleScale s =
    case s of
        Text _ _ -> s
        Line _ _ -> s
        Rectangle o w h -> Rectangle o (w * 2.0) (h * 2.0)
        Circle o r -> Circle o $ r * 2.0
        Clipped o p w h -> Clipped o p (w * 2.0) (h * 2.0)

getText :: Shape -> Maybe String
getText = case _ of
    Text _ t -> Just t
    _ -> Nothing

area :: Shape -> Number
area = case _ of
    Text _ _ -> 0.0
    Line _ _ -> 0.0
    Rectangle _ w h -> w * h
    Circle _ r -> r * r * Math.pi
    Clipped _ _ w h -> w * h
