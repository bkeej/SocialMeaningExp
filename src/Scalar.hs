{-
 - RSA model for scalar implicature, with lexical uncertainty and null-message
 - costs (and an idle but fiddle-with-able temperature parameter).
 -
 - The model defined below is intended to be equivalent to that of Potts et
 - al. 2015[^1] (specifically, the extended model in their Appendix A, of
 - which the model in the main text is a special case).
 -
 - Running `disp_s 1` reproduces table s_1 in the left-most column of their
 - Figure 2 (page 773), and running `disp_L 1` reproduces table L at the top
 - of their Figure 2 (modulo rounding).
 -
 - To do: Think about more principled and/or general approaches to `modify`;
 - look at local implicature generation; think about how to automatically
 - generate refined lexica; play with exceptional implicature.
 -
 - [^1]: https://doi.org/10.1093/jos/ffv012
-}

module Scalar where

import           Control.Monad             (guard)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Data.Function             (on)
import           Data.List                 (groupBy, partition, sortBy)
import           Prob
import           Utils

--
-- Model-theoretic stuff
--

data World = N | S | A
  deriving (Show, Eq, Enum, Ord)

data Message = Some | All | Null
  deriving (Show, Eq, Enum, Ord)

type Lexicon = Message -> [World]

eval :: Lexicon
eval Some = [S, A]
eval All  = [A]
eval Null = [N, S, A]

--
-- Model parameters
--

cost Null = 5   -- Only null messages incur costs
cost _    = 0   -- Coerced into Sum's by GHC?

temperature = 1 -- Higher values ~> more eager pragmatic reasoning

worldPrior :: Dist m => m World
worldPrior = uniform [N ..]       -- The primitive notion in Potts et al. is a
                                  -- prior over *sets* of worlds/states (i.e.,
                                  -- propositions).

messagePrior :: Dist m => m Message
messagePrior = uniform [Some ..]  -- No correspondent in Potts et al. Flat
                                  -- here so makes no difference. May be able
                                  -- to play a role analogous to msg costs?

refineEval1 :: Lexicon
refineEval1 m
  | m == Some = [S]
  | otherwise = eval m  -- (null messages aren't refined)

refineEval2 :: Lexicon
refineEval2 m
  | m == Some = [A]
  | otherwise = eval m  -- (null messages aren't refined)

lexiconPrior :: Dist m => m Lexicon
lexiconPrior = uniform [eval, refineEval1, refineEval2]
                                  -- Like worlds, the primitive thing in Potts
                                  -- et al. is a prior over *sets* of lexica.

--
-- Arbitrarily pragmatic agents given by mutually recursive functions.
-- The definitions here are somewhat more general than those of Potts et al.
--

listener :: Int -> Message -> Lexicon -> BDDist World
listener n m sem = bayes $ do
  w <- worldPrior
  if n <= 0   -- literal listener
    then guard (w `elem` sem m)
    else do   -- pragmatic listener
      m' <- speaker n w sem
      guard (m' == m)
  return w

speaker :: Int -> World -> Lexicon -> BDDist Message
speaker n w sem = bayes $ do
  m  <- messagePrior
  w' <- scaleProb m (listener (n-1) m sem)
  guard (w' == w)
  return m

-- Helper functions for scaling probabilities
scaleProb :: Message -> BDDist a -> BDDist a
scaleProb m = modify (exp . (temperature *) . subtract (cost m) . log)

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

--
-- Variable-lexica agents
--

lexicaSpeaker :: Int -> World -> BDDist Message
lexicaSpeaker n w = bayes $ do
  m  <- messagePrior
  w' <- scaleProb m (lexicaListener (n-1) m)
  guard (w' == w)
  return m

lexicaListener :: Int -> Message -> BDDist World
lexicaListener n m = weightedEq . runMassT . bayes $ do
  w  <- worldPrior
  m' <- if n <= 1
          then do
            lex <- lexiconPrior
            speaker n w lex
          else lexicaSpeaker n w
  guard (m' == m)
  return w

-- Sum weights of identical outcomes
weightedEq :: (Dist m, Eq a) => [Mass Prob a] -> m a
weightedEq vs = weighted (concatMap col bins)
  where bins = groupEqBy ((==) `on` getSndMass) vs
        col []                = []
        col ms@((Mass _ x):_) = [Mass (sum (map getFstMass ms)) x]

groupEqBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupEqBy _ [] = []
groupEqBy f (a:rest) = (a:as) : groupEqBy f bs
  where (as,bs) = partition (f a) rest

--
-- Testing the model
--

pretty o mx = dropR 2 probs where
  probs = "P(.|" ++ show o ++"): " ++ concatMap f (runMassT (runMaybeT mx))
  f = \(Mass n (Just x)) -> show x ++ " = " ++ prettyN n ++ ", "
  prettyN (Sum n) = show (fromInteger (round (n * 100)) / 10.0^^2)
  dropR n xs = fst (splitAt (length xs - n) xs)

disp_s n = sequence_ (map putStrLn test)
  where test = [pretty w (speaker n w eval)   | w <- [N ..]]

disp_l n = sequence_ (map putStrLn test)
  where test = [pretty m (listener n m eval)  | m <- [Some ..]]

disp_L n = sequence_ (map putStrLn test)
  where test = [pretty m (lexicaListener n m) | m <- [Some ..]]
