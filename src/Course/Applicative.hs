{-# LANGUAGE NoImplicitPrelude #-}

module Course.Applicative(
  Applicative(..)
, sequence
, replicateA
, filtering
) where

import Course.Core
import Course.Apply
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class Apply f => Applicative f where
  pure :: a -> f a

-- | Witness that all things with (<*>) and pure also have (<$>).
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$>) :: Applicative f => (a -> b) -> f a -> f b
-- (<$>) f fa = (pure f) <*> fa 
(<$>) = (<*>).pure
-- | Insert into the Id monad.
--
-- prop> pure x == Id x
instance Applicative Id where
	--  a -> Id a
  pure = Id

-- | Insert into a List.
--
-- prop> pure x == x :. Nil
instance Applicative List where
  pure a = a:.Nil  

-- | Insert into an Optional.
--
-- prop> pure x == Full x
instance Applicative Optional where
  pure a = Full a

-- | Insert into a constant function.
--
-- prop> pure x y == x
instance Applicative ((->) t) where
 --  a -> t -> a
  pure = const

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (Id 7 :. Id 8 :. Id 9 :. Nil)
-- Id [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence :: Applicative f => List (f a) -> f (List a)
sequence = foldRight (\fa facc -> lift2 (:.) fa facc) (pure Nil)
-- Inside f world
-- 	a :. acc :. aacc
-- 	
-- Ouside f world
--     f aacc
-- 
-- acc = pure (a1:.Nil)	
-- acc = pure (a2:.a1:.Nil)

--  el :: f a
--  acc :: f (List a)
--  result :: f (List a)



-- | Replicate an effect a given number of times.
--
-- >>> replicateA 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
replicateA :: Applicative f => Int -> f a -> f (List a)
replicateA num func = sequence (replicate num func)  

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (Id . even) (4 :. 5 :. 6 :. Nil)
-- Id [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)
filtering _ Nil = pure Nil
filtering y (h:.t) = lift2 (\a b -> if a then h:.b else b) (y h) (filtering y t)
--       												   f bool f (List a)
-- 															a          b
-- (a -> b -> c) -> f a -> f b -> f c
-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return

instance Applicative [] where
  pure =
    P.return

instance Applicative P.Maybe where
  pure =
    P.return
