{-
 - Implements a simple IBR/RSA-type model using BDDist (Prob.hs), sans costs
 - and temperatures. See Monroe & Potts 2015[^1] (Figure 1) for the model
 - assumed here. Running `test!!1` reproduces the table in their Figure (1d).
 -
 - [^1]: https://nlp.stanford.edu/pubs/monroe2015learning.pdf
-}

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Prob
import           Utils

--
-- Model-theoretic stuff
--

data World = R1 | R2 | R3
  deriving (Show, Eq, Enum)

data Message = Beard | Glasses | Tie
  deriving (Show, Eq, Enum)

worldPrior :: Dist m => m World
worldPrior = uniform [R1 ..]

messagePrior :: Dist m => m Message
messagePrior = uniform [Beard ..]

eval :: (Message, World) -> Bool
eval (Beard,   R1) = True
eval (Beard,   _ ) = False
eval (Glasses, R3) = False
eval (Glasses, _ ) = True
eval (Tie,     R1) = False
eval (Tie,     _ ) = True

--
-- Mutually recursive pragmatic reasoning
--

speaker :: Int -> World -> BDDist Message
speaker n w = lift . bayes $ do
  m <- messagePrior
  if n <= 0   -- literal speaker
    then do
      guard $ eval (m, w)
      return m
    else do   -- arbitarily pragmatic speaker
      w' <- listener n m
      guard $ w' == w
      return m

listener :: Int -> Message -> BDDist World
listener n m = lift . bayes $ do
  w  <- worldPrior
  m' <- speaker (n-1) w
  guard $ m' == m
  return w

--
-- Testing the model
--

test = [[runMassT (runMaybeT (speaker n w)) | w <- [R1 ..]] | n <- [0..]]

{-
 - test!!0 = literal speaker
 - test!!1 = pragmatic speaker
 - test!!n = n-pragmatic speaker
 - gets intractable around test!!5 (!)
-}
