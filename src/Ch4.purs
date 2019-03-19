module Ch4 where 

import Prelude
import Control.MonadZero (guard)
import Data.Array ((..), (:), head, drop, filter, concat, length, foldl, concatMap)
import Data.Foldable (product)
import Data.Maybe (Maybe(..))
import Data.Path
import Math (sqrt)

isEven :: Int -> Boolean
isEven = case _ of 
    0 -> true 
    1 -> false 
    n -> n - 2 # isEven

evenCount :: Array Int -> Int
evenCount arr = 
    case head arr of 
        Nothing -> 
            0
        Just x ->
            if isEven x then 
                1 + evenCount (drop 1 arr)
            else 
                evenCount $ drop 1 arr

squares :: Array Number -> Array Number 
squares = map \n -> n * n

noNegatives :: Array Number -> Array Number 
noNegatives arr = (\n -> n >= 0.0) <$?> arr

infix 8 filter as <$?>

factors :: Int -> Array (Array Int)
factors n = do 
    i <- 1 .. n 
    j <- i .. n 
    guard $ i * j == n
    pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do 
    x <- xs 
    y <- ys 
    pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do 
    a <- 1..n
    b <- 1..n 
    c <- 1..n 
    guard $ a * a + b * b == c * c && a <= b
    pure [a,b,c]

factorizations :: Int -> Array (Array Int)
factorizations n = do
    x <- 1 .. n 
    y <- 1 .. x 
    guard $ x * y == n
    pure [x, y]

allTrue :: Array Boolean -> Boolean 
allTrue =
    foldl (&&) true

-- ex 2: arrays with a first element of false and the rest as true.

count :: forall a. (a -> Boolean) -> Array a -> Int 
count f =
    count_ 0
    where
        count_ c arr = case head arr of 
            Nothing -> c
            Just x -> if f x 
                then count_ (c + 1) (drop 1 arr) 
                else count_ c (drop 1 arr)

reverse :: forall a. Array a -> Array a
reverse =
    foldl (\acc x -> [x] <> acc) []


-- file ops

allFiles :: Path -> Array Path 
allFiles file = file : concatMap allFiles (ls file)

allFiles_ :: Path -> Array Path 
allFiles_ file = file : do 
    child <- ls file 
    allFiles_ child

onlyFiles :: Path -> Array Path
onlyFiles file = do 
    child <- ls file 
    if not $ isDirectory child 
        then pure child 
        else onlyFiles child

smallest :: Path -> Maybe Int 
smallest path = 
    onlyFiles path # foldl newSize Nothing
    where newSize acc f = 
            case acc of 
                Nothing -> size f 
                Just s -> case size f of
                    Nothing -> Just s 
                    Just s_ -> if s_ < s then Just s_ else Just s

biggest :: Path -> Maybe Int 
biggest path =
    onlyFiles path # foldl newSize Nothing 
    where newSize acc f = 
            case acc of 
                Nothing -> size f 
                Just s -> case size f of
                    Nothing -> Just s 
                    Just s_ -> if s_ > s then Just s_ else Just s

whereIs :: String -> Maybe Path 
whereIs name = head do 
    file <- allFiles_ root
    guard $ isDirectory file 
    do child <- ls file 
       guard $ filename child == name 
       pure file
