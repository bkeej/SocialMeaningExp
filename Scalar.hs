{-
 - Implements a simple IBR/RSA-type model using BDDist (Prob.hs), sans costs
 - and temperatures. See Monroe & Potts 2015 [^1] (Figure 1) for the model
 - assumed here. Running `test!!1` reproduces the table in their Figure (1d).
 -
 - [^1]: https://nlp.stanford.edu/pubs/monroe2015learning.pdf
-}

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Prob
import           Utils

-- Model-theoretic stuff

data World = N | S | A
  deriving (Show, Eq, Enum)

data Message = Some | All | NullMsg
  deriving (Show, Eq, Enum)

worldPrior :: Dist m => m World
worldPrior = uniform [N ..]

messagePrior :: Dist m => m Message
messagePrior = uniform [Some ..]

eval :: (Message, World) -> Bool
eval (Some   , N) = False
eval (Some   , _) = True
eval (All    , A) = True
eval (All    , _) = False
eval (NullMsg, _) = True

-- Mutually recursive pragmatic reasoning

listener :: Int -> Message -> BDDist World
listener n m = lift . bayes $ do
  w <- worldPrior
  if n <= 0   -- literal listener
    then do
      guard $ eval (m, w)
      return w
    else do   -- arbitarily pragmatic listener
      m' <- speaker n w
      guard $ m' == m
      return w

speaker :: Int -> World -> BDDist Message
speaker n w = lift . bayes $ do
  m  <- messagePrior
  w' <- listener (n-1) m
  guard $ w' == w
  return m

-- testing the model

test = [[runMassT (runMaybeT (speaker n w)) | w <- [N ..]] | n <- [0..]]

{-
 - test!!0 = literal speaker
 - test!!1 = pragmatic speaker
 - test!!n = n-pragmatic speaker
 - gets intractable around test!!5 (!)
-}
