{-# LANGUAGE Haskell2010
  , BangPatterns
  , PatternSynonyms
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}

{- | Solution (meant to be loaded in GHCi) -}
module Solution
  ( ) where


-- + Imports

-- ++ Local:

import Data.Strict.Tuple
  ( pattern Tup2
  , Tup3
      ( Tup3
      , of3ind0
      )
  )

import Data.Stream
  ( Stream
  , unfold
  , index
  , repTime
  )


-- * Solution (meant to be loaded in GHCi)

modPowEventualPeriod :: forall a. Integral a => a -> a -> Int
modPowEventualPeriod = \ m n ->
    let purify = \ m' -> case gcd m' n of
            1 -> m'
            x -> purify $ quot m' x
        !m_ess = purify m
    in  repTime $ unfold (\ !n' -> Tup2 n' (rem (n * n') m_ess)) $ rem n m_ess

coeff0, coeff1, coeff2 :: Int
coeff0 = 3
coeff1 = 1
coeff2 = 0

seed :: Tup3 Int Int Int
seed = Tup3 3 coeff2 (coeff2 * coeff2 + 2 * coeff1)

base :: Int
base = 100

sequence0 :: Stream (Tup3 Int Int Int)
sequence0 = unfold (\ t@(Tup3 n0 n1 n2) -> Tup2 t . Tup3 n1 n2 . (`rem` base) $ coeff0 * n0 + coeff1 * n1 + coeff2 * n2) seed

period0 :: Int
period0 = repTime sequence0

exp0, exp1, exp2 :: Int
exp0 = 2027
exp1 = 2026
exp2 = 2025

sequence1 :: Stream Int
sequence1 = unfold (\ !n -> Tup2 n . (`rem` period0) $ exp0 * n) 1

period1 :: Int
period1 = modPowEventualPeriod period0 exp0

sequence2 :: Stream Int
sequence2 = unfold (\ !n -> Tup2 n . (`rem` period1) $ exp1 * n) 1

answer :: Int
answer = index (fmap of3ind0 sequence0) . index sequence1 $ index sequence2 exp2
