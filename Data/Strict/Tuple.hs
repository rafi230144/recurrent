{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , PatternSynonyms
  , ScopedTypeVariables
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Strict pairs and triples\; could be done w/ TH instead #-}
module Data.Strict.Tuple
  ( Tup2
      ( Tup2
      , of2ind0
      , of2ind1
      )
  , Tup3
      ( Tup3
      , of3ind0
      , of3ind1
      , of3ind2
      )
  ) where


-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )


-- * Strict pairs and triples\; could be done w/ TH instead

{- | Strict pairs -}
data Tup2 :: Type -> Type -> Type where
    Tup2 :: forall a0 a1. {
        of2ind0 :: !a0 ,
        of2ind1 :: !a1 } ->
        Tup2 a0 a1

deriving instance forall a0 a1. (Eq a0, Eq a1) => Eq (Tup2 a0 a1)

{- | Strict triples -}
data Tup3 :: Type -> Type -> Type -> Type where
    Tup3 :: forall a0 a1 a2. {
        of3ind0 :: !a0 ,
        of3ind1 :: !a1 ,
        of3ind2 :: !a2 } ->
        Tup3 a0 a1 a2

deriving instance forall a0 a1 a2. (Eq a0, Eq a1, Eq a2) => Eq (Tup3 a0 a1 a2)
