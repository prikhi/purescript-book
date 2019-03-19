module Ch6 where

import Prelude
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldr, foldl, foldMap, maximum)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Picture (Shape, showShape)


newtype Shape_ = Shape_ Shape
instance showShape_ :: Show Shape_ where
    show (Shape_ s) = showShape s


newtype Complex
    = Complex
        { real :: Number
        , imaginary :: Number
        }

instance showComplex :: Show Complex where
    show (Complex { real, imaginary }) =
        show real <> "+" <> show imaginary <> "i"

instance eqComplex :: Eq Complex where
    eq (Complex c1) (Complex c2) =
        c1 == c2


threeEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeEqual a b c = a == b && b == c


data NonEmpty a
    = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
    eq (NonEmpty a1 arr1) (NonEmpty a2 arr2) = a1 == a2 && arr1 == arr2

instance semiNonEmpty :: Semigroup (NonEmpty a) where
    append (NonEmpty a1 arr1) (NonEmpty a2 arr2) =
        NonEmpty a1 $ arr1 <> [a2] <> arr2

instance functorNonEmpty :: Functor NonEmpty where
    map f (NonEmpty a arr) =
        NonEmpty (f a) $ map f arr

instance foldableNonEmpty :: Foldable NonEmpty where
    foldr f acc (NonEmpty a arr) = foldr f acc $ [a] <> arr
    foldl f acc (NonEmpty a arr) = foldl f acc $ [a] <> arr
    foldMap f (NonEmpty a arr) = foldMap f $ [a] <> arr


data Extended a
    = Finite a
    | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
    eq (Finite x) (Finite y) = x == y
    eq Infinite Infinite = true
    eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
    compare (Finite x) (Finite y) =
        compare x y
    compare Infinite Infinite = EQ
    compare Infinite _ = GT
    compare _ Infinite = LT


data OneMore f a
    = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
    foldl f acc (OneMore a fa) = foldl f (f acc a) fa
    foldr f acc (OneMore a fa) = f a $ foldr f acc fa
    foldMap f (OneMore a fa) = f a <> foldMap f fa


partialMaximum :: Partial => Array Int -> Int
partialMaximum arr = fromJust $ maximum arr


newtype Multiply = Multiply Int
instance semigroupMulitply :: Semigroup Multiply where
    append (Multiply n) (Multiply m) = Multiply (n * m)
instance monoidMultiple :: Monoid Multiply where
    mempty = Multiply 1

class Monoid m <= Action m a where
    act :: m -> a -> a
instance repeatAction :: Action Multiply String where
    act (Multiply 1) a = a
    act (Multiply n) a = a <> act (Multiply $ n - 1) a
instance arrayAction :: Action m a => Action m (Array a) where
    act m arr = map (act m) arr

newtype Self m = Self m
instance selfAction :: Monoid m => Action m (Self m) where
    act m1 (Self m2) = Self $ m1 <> m2


newtype HashCode = HashCode Int

instance eqHash :: Eq HashCode where
    eq (HashCode h1) (HashCode h2) = h1 == h2

hashCode :: Int -> HashCode
hashCode h = HashCode $ h `mod` 65535

class Eq a <= Hashable a where
    hash :: a -> HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
    hash = hashCode

instance hashBool :: Hashable Boolean where
    hash = case _ of
        true -> hashCode 1
        false -> hashCode 0

instance hashArray :: Hashable a => Hashable (Array a) where 
    hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashChar :: Hashable Char where
    hash = hashCode <<< toCharCode
