{-
 - Implements a simple IBR/RSA-type model using BDDist (Prob.hs), sans costs
 - and temperatures. See Monroe & Potts 2015 [^1] (Figure 1) for the model
 - assumed here. Running test!!1 reproduces the table in their Figure (1d).
 -
 - [^1]: https://nlp.stanford.edu/pubs/monroe2015learning.pdf
-}

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Prob
import           Utils

-- Model-theoretic stuff

data World = R1 | R2 | R3
  deriving (Show, Eq, Enum)

data Message = Beard | Glasses | Tie
  deriving (Show, Eq, Enum)

worldPrior :: Dist m => m World
worldPrior = uniform [R1 ..]

messagePrior :: Dist m => m Message
messagePrior = uniform [Beard ..]

lang :: (Message, World) -> Bool
lang (Beard,   R1) = True
lang (Beard,   _ ) = False
lang (Glasses, R3) = False
lang (Glasses, _ ) = True
lang (Tie,     R1) = False
lang (Tie,     _ ) = True

-- Dually recursive pragmatic reasoning

s :: Int -> World -> BDDist Message
s n w = lift . bayes $ do
  m <- uniform [Beard ..]
  if n <= 0
    then do
      guard $ lang (m, w)
      return m
    else do
      w' <- l n m
      guard $ w' == w
      return m

l :: Int -> Message -> BDDist World
l n m = lift . bayes $ do
  w  <- uniform [R1 ..]
  m' <- s (n-1) w
  guard $ m' == m
  return w

-- testing the model

test = [[runMassT (runMaybeT (s n w)) | w <- [R1 ..]] | n <- [0..]]

{-
 - test!!0 = literal speaker
 - test!!1 = pragmatic speaker
 - test!!n = n-pragmatic speaker
 - gets intractable around test!!5 (!)
-}
