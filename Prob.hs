{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prob where

import           Control.Applicative
import           Control.Monad             (guard, replicateM)
import           Control.Monad.Trans.Maybe
import           Data.List                 (group, sort)
import           Data.Maybe                (catMaybes)
import           System.Random             (getStdRandom, random)
import           Utils

--
-- Dist monads support normalization
--

type Prob = Sum Float

class Monad m => Dist m where
  weighted :: [Mass Prob a] -> m a

uniform :: Dist m => [a] -> m a
uniform = weighted . map (\x -> Mass 1 x)

coin :: Dist m => Float -> a -> a -> m a
coin n x1 x2 = weighted [Mass (Sum n) x1, Mass (Sum (1-n)) x2]

--
-- DDist
--

type DDist = MassT Prob []

instance Dist DDist where
  weighted vs = MassT [Mass (n / total) x | Mass n x <- vs, total /= 0]
    where total = sum (map getFstMass vs)

--
-- MC sampling
--

newtype MC a = MC { runMC :: IO a }
  deriving (Functor, Applicative, Monad)

randomFloat :: MC Float
randomFloat = MC (getStdRandom (random))

instance Dist MC where
  weighted = liftF . weighted

liftF :: DDist a -> MC a
liftF ddist = do
  n <- randomFloat
  pick (Sum n) (runMassT ddist)

pick :: Dist m => Prob -> [Mass Prob a] -> m a
pick _ [] = fail "No values to pick from"
pick n ((Mass m x):vs)
  | n <= m    = return x
  | otherwise = pick (n-m) vs

sample :: Int -> MC a -> MC [a]
sample = replicateM

--
-- Bayesian filtering
--

instance Dist m => Dist (MaybeT m) where
  weighted = MaybeT . fmap Just . weighted

--
-- Bayesian DDist
--

type BDDist = MaybeT DDist

bayes :: BDDist a -> DDist a
bayes = weighted . catMaybes' . runMassT . runMaybeT
  where catMaybes' = catMaybes . fmap pull
        pull (Mass y mx) = do {x <- mx; return (Mass y x)}

--
-- Bayesian MC
--

type BMC = MaybeT MC

sampleWithRejections :: Int -> BMC a -> MC [a]
sampleWithRejections n d = fmap catMaybes (sample n (runMaybeT d))

bayesMC :: Ord a => Int -> BMC a -> IO [Int]
bayesMC n v = fmap hist (runMC (sampleWithRejections n v))
  where hist = map length . group . sort

--
-- Flu test
--

data Status = Flu | Healthy
  deriving (Show, Eq, Ord)
data Test   = Pos | Neg
  deriving (Show, Eq, Ord)

statusCondPos :: (Dist m, Alternative m) => m (Status, Test)
statusCondPos = do
  status <- coin 0.10 Flu Healthy
  test   <-
    if (status == Flu)
      then coin 0.70 Pos Neg
      else coin 0.10 Pos Neg
  guard (test == Pos)
  return (status, test)

test1 = runMassT (bayes statusCondPos)
test2 = fmap (\ns -> [fromIntegral n / fromIntegral (sum ns) | n <- ns]) nsIO
  where nsIO = bayesMC 10000 statusCondPos
