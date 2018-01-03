{-
 - RSA model for scalar implicature, with lexical uncertainty and null message
 - costs (but no temperature parameters).
 -
 - See Potts et al. 2015[^1] (Figure 2) for the model assumed here. Running
 - `disp_s1` reproduces table s_1 in the left-most column of their Figure 2
 - (page 773), modulo rounding. Running `disp_L` reproduces table L at the top
 - of their Figure 2, modulo rounding.
 -
 - To do: Think about more principled and/or general approaches to `modify`,
 - `weightedEq`; look at local implicature generation; make sure that
 - refinements of null messages are immaterial (and think about how to
 - automatically generate refined lexica).
 -
 - [^1]: https://doi.org/10.1093/jos/ffv012
-}

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.List
import           Prob
import           Utils

--
-- Model-theoretic stuff
--

data World = N | S | A
  deriving (Show, Eq, Enum, Ord)

data Message = Some | All | Null
  deriving (Show, Eq, Enum, Ord)

worldPrior :: Dist m => m World
worldPrior = uniform [N ..]

messagePrior :: Dist m => m Message
messagePrior = uniform [Some ..]

cost :: Num a => Message -> a
cost Null = 5
cost _    = 0  -- coerced into Sum's by GHC?

type Lexicon = Message -> [World]

eval :: Lexicon
eval Some = [S, A]
eval All  = [A]
eval Null = [N, S, A]

refineEval1 :: Lexicon
refineEval1 m
  | m == Some = [S]
  | otherwise = eval m

refineEval2 :: Lexicon
refineEval2 m
  | m == Some = [A]
  | otherwise = eval m

lexiconPrior :: Dist m => m Lexicon
lexiconPrior = uniform [eval, refineEval1, refineEval2]

powersetPlus :: Eq a => [a] -> [[a]]
powersetPlus = filter (/= []) . powerset
  where powerset []     = [[]]
        powerset (x:xs) = let xss = powerset xs in xss ++ map (x:) xss

refineEval :: Message -> [[World]]
refineEval = powersetPlus . eval

--
-- Mutually recursive pragmatic reasoning
--

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

scaleProb :: Message -> BDDist a -> BDDist a
scaleProb m = modify (exp . subtract (cost m) . log)

listener :: Int -> Message -> Lexicon -> BDDist World
listener n m sem = lift . bayes $ do
  w <- worldPrior
  if n <= 0   -- literal listener
    then do
      guard (w `elem` sem m)
      return w
    else do   -- pragmatic listener
      m' <- speaker n w sem
      guard (m' == m)
      return w

speaker :: Int -> World -> Lexicon -> BDDist Message
speaker n w sem = lift . bayes $ do
  m  <- messagePrior
  w' <- scaleProb m (listener (n-1) m sem)
  guard (w' == w)
  return m

finalListener :: Message -> BDDist World
finalListener m = lift . (weightedEq . runMassT) . bayes $ do
  w   <- worldPrior
  lex <- lexiconPrior
  m'  <- speaker 1 w lex
  guard (m' == m)
  return w

weightedEq :: (Dist m, Ord a) => [Mass Prob a] -> m a
weightedEq vs = weighted (concatMap col bins)
  where vsOrd = sortBy  (\x y -> compare (getSndMass x) (getSndMass y)) vs
        bins  = groupBy (\x y -> (==)    (getSndMass x) (getSndMass y)) vsOrd
        col []                = []
        col ms@((Mass _ x):_) = [Mass (sum (map getFstMass ms)) x]

--
-- Testing the model
--

disp_s1 = sequence_ (map print (test!!1))
  where test = [[pretty w (speaker n w eval) | w <- [N ..]] | n <- [0..]]

disp_L = sequence_ (map print test)
  where test = [pretty m (finalListener m) | m <- [Some ..]]

pretty o mx = ["P("++ show x ++"|"++ show o ++") = "++ show (getSum n) |
  Mass n (Just x) <- runMassT (runMaybeT mx)]
