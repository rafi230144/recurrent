{-# LANGUAGE Haskell2010
  , BangPatterns
  , GADTSyntax
  , InstanceSigs
  , KindSignatures
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Streams strict in their elements\;
    were this compiled, we'd ought to set up fusion!
-}
module Data.Stream
  ( Stream
      ( Cons
      , head
      , tail
      )
  , unfold
  , fold
  , index
  , toList -- for debugging in GHCi
  , repTime
  ) where


-- + Imports

-- ++ From base:

import Prelude hiding
  ( head
  , tail
  )

import Data.Kind
  ( Type )


-- ++ Local:

import Data.Strict.Tuple
  ( Tup2
      ( Tup2 )
  )


-- * Streams strict in their elements

{- | Streams strict in their elements -}
data Stream :: Type -> Type where
    Cons :: forall a. {
        head :: !a,
        tail :: Stream a } ->
        Stream a

unfold :: forall a s. (s -> Tup2 a s) -> s -> Stream a
unfold = \ x s -> case x s of Tup2 a s' -> Cons a $ unfold x s'

fold :: forall a b. (a -> b -> b) -> Stream a -> b
fold = \ g ~(Cons a sa') -> g a $ fold g sa'

instance Functor Stream where
    fmap :: forall a0 a1. (a0 -> a1) -> Stream a0 -> Stream a1
    fmap = \ f -> fold (\ a -> Cons $ f a)

index :: forall a. Stream a -> Int -> a
index = \ sa n ->
    fold (\ a k !n' -> case n' of
        0 -> a
        _ -> k $ n' - 1
      ) sa n

toList :: forall a. Stream a -> [a]
toList = fold (\ a -> (:) a)

{- | Time to first repeat of head -}
repTime :: forall a. Eq a => Stream a -> Int
repTime = \ (Cons a0 sa'0) ->
    fold (\ a k !n -> case a0 == a of
        True  -> n
        False -> k $ n + 1
      ) sa'0 1
