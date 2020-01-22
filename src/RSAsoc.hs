{-
 - Implements a simple RSA-type model using BDDist (Prob.hs), sans costs and
 - temperatures. See Monroe & Potts 2015[^1] (Figure 1) for the model assumed
 - here. Running `disp_s 1` reproduces the table in their Figure (1d).
 -
 - [^1]: https://nlp.stanford.edu/pubs/monroe2015learning.pdf
-}

module RSAsoc where

import           Control.Monad             (guard)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Prob
import           Utils

--
-- Model-theoretic stuff
--

data Property = P String
  deriving (Show, Eq)

-- Social indices are lists of properties, intended to be 
-- mutually-inconsistent e.g., P articulate / P inarticulate
type Properties = [Property]

-- Indexical Fields are sets of properties
type IField = [Properties]

-- Reserve the term Eckert-Montague Fields 
-- for maximal consistent subsets of some set of properties. 
type EMField = [Properties]

-- Personae generates EMFields from IFields
personae :: IField -> EMField
personae p = sequence p

--Reserve Persona for members of an EMField
type Persona = Properties

data World = R1 | R2 | R3
  deriving (Show, Eq, Enum)

data Message = Beard | Glasses | Tie
  deriving (Show, Eq, Enum)

type Lexicon = Message -> [World]

eval :: Lexicon
eval Beard   = [R1]
eval Glasses = [R1, R2]
eval Tie     = [R2, R3]

--
-- Model parameters
--

cost _ = 0      -- Across-the-board no-cost messages

temperature = 1 -- Higher values ~> more eager pragmatic reasoning

worldPrior :: Dist m => m World
worldPrior = uniform [R1 ..]

messagePrior :: Dist m => m Message
messagePrior = uniform [Beard ..]

--
-- Mutually recursive pragmatic reasoning
--

speaker :: Int -> World -> Lexicon -> BDDist Message
speaker n w sem = bayes $ do
  m <- messagePrior
  scaleProb m $ if n <= 0   -- literal speaker
                  then guard (w `elem` sem m)
                  else do   -- pragmatic speaker
                    w' <- listener n m sem
                    guard (w' == w)
  return m

listener :: Int -> Message -> Lexicon -> BDDist World
listener n m sem = bayes $ do
  w  <- worldPrior
  m' <- speaker (n-1) w sem
  guard (m' == m)
  return w

-- Helper functions for scaling probabilities
scaleProb :: Message -> BDDist a -> BDDist a
scaleProb m = modify (exp . (temperature *) . subtract (cost m) . log)

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

--
-- Testing the model
--

disp_s n = sequence_ (map print test)
  where test = [pretty w (speaker n w eval)   | w <- [R1 ..]]

disp_l n = sequence_ (map print test)
  where test = [pretty m (listener n m eval)  | m <- [Beard ..]]

pretty o mx = "P(.|"++ show o ++"): "++ concat [show x ++" = "++ show (getSum
  n) ++", " | Mass n (Just x) <- runMassT (runMaybeT mx)]
