{-
 - RSA model for scalar implicature, with a costly null message. See Potts et
 - al. 2015[^1] (Figure 2) for the model assumed here. Running `test!!1`
 - reproduces s_1 in the left-most column of their Figure 2.
 -
 - Will eventually add lexical uncertainty, maybe fold `modify` into Util.
-}

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Prob
import           Utils

--
-- Model-theoretic stuff
--

data World = N | S | A
  deriving (Show, Eq, Enum)

data Message = Some | All | NullMsg
  deriving (Show, Eq, Enum)

worldPrior :: Dist m => m World
worldPrior = uniform [N ..]

messagePrior :: Dist m => m Message
messagePrior = uniform [Some ..]

cost NullMsg = 5
cost _       = 0  -- coerced into Sum's by the compiler?

eval :: (Message, World) -> Bool
eval (Some   , N) = False
eval (Some   , _) = True
eval (All    , A) = True
eval (All    , _) = False
eval (NullMsg, _) = True

--
-- Mutually recursive pragmatic reasoning
--

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT fd)
  where fd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

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
  w' <- modify (\w -> exp (log w - cost m)) (listener (n-1) m)
  guard $ w' == w
  return m

--
-- Testing the model
--

test = [[runMassT (runMaybeT (speaker n w)) | w <- [N ..]] | n <- [0..]]

{-
 - test!!1 = pragmatic speaker (note: test!!0 == test!!1)
 - test!!n = n-pragmatic speaker
 - gets intractable around test!!5 (!)
-}
