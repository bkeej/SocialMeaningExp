{-
 - Implements extention of Burnett-style Social Meaning Games[^1] for dogwhistles 
 - following work by Henderson and McCready[^2]. Based on RSA.hs. 
 -
 - [^1]: https://www.rhenderson.net/resources/papers/how_dogwhistles_work.pdf
 - [^2]: http://www.heatherburnett.net/uploads/9/6/6/0/96608942/jofs_burnett.pdf
-}

module RSAsoc where

import           Control.Monad             (guard)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Prob
import           Utils

--
-- Model structure
--

data Property = P String
  deriving (Show, Eq)

-- Social indices are lists of properties, intended to be 
-- mutually-inconsistent e.g., P articulate / P inarticulate.
type Properties = [Property]

-- Indexical Fields are sets of properties
type IField = [Properties]

-- Reserve the term Eckert-Montague Fields for maximal 
-- consistent subsets of some set of properties.
type EMField = [Properties]

-- Personae generates EMFields from IFields
personae :: IField -> EMField
personae p = sequence p

-- Reserve Persona for members of an EMField
type Persona = Properties

data Message = M String
  deriving (Show, Eq)

--Messages are not interpreted in worlds, but bear social meaning, 
--namely they denote the set of personas they are consistent with.
type Lexicon = Message -> EMField -> [Persona]

-- Return the set of personas in the EMField consistent 
-- with message M x.
eval :: Lexicon
eval (M x) f = [i | i <- f, 
                (P p) <- i, 
                p==x]

--
-- Stein example model from Henderson & McCready 2018
--

properties = [[P "AntiVax", P "ProVax"], [P "AntiCorp", P "ProCorp"]]

emfield = personae properties

messages = [M "AntiVax", M "ProVax", M "AntiCorp", M "ProCorp"]

{- 
data World = R1 | R2 | R3
deriving (Show, Eq, Enum)

data Message = Beard | Glasses | Tie
deriving (Show, Eq, Enum)

eval :: Lexicon
eval Beard   = [R1]
eval Glasses = [R1, R2]
eval Tie     = [R2, R3]
-}

--
-- Model parameters
--

-- cost _ = 0      -- Across-the-board no-cost messages

-- temperature = 1 -- Higher values ~> more eager pragmatic reasoning

-- worldPrior :: Dist m => m World
-- worldPrior = uniform [R1 ..]

-- messagePrior :: Dist m => m Message
-- messagePrior = uniform [Beard ..]

-- --
-- -- Mutually recursive pragmatic reasoning
-- --

-- speaker :: Int -> World -> Lexicon -> BDDist Message
-- speaker n w sem = bayes $ do
--   m <- messagePrior
--   scaleProb m $ if n <= 0   -- literal speaker
--                   then guard (w `elem` sem m)
--                   else do   -- pragmatic speaker
--                     w' <- listener n m sem
--                     guard (w' == w)
--   return m

-- listener :: Int -> Message -> Lexicon -> BDDist World
-- listener n m sem = bayes $ do
--   w  <- worldPrior
--   m' <- speaker (n-1) w sem
--   guard (m' == m)
--   return w

-- -- Helper functions for scaling probabilities
-- scaleProb :: Message -> BDDist a -> BDDist a
-- scaleProb m = modify (exp . (temperature *) . subtract (cost m) . log)

-- modify :: (Prob -> Prob) -> BDDist a -> BDDist a
-- modify f mx = MaybeT (MassT f'd)
--   where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

-- --
-- -- Testing the model
-- --

-- disp_s n = sequence_ (map print test)
--   where test = [pretty w (speaker n w eval)   | w <- [R1 ..]]

-- disp_l n = sequence_ (map print test)
--   where test = [pretty m (listener n m eval)  | m <- [Beard ..]]

-- pretty o mx = "P(.|"++ show o ++"): "++ concat [show x ++" = "++ show (getSum
--   n) ++", " | Mass n (Just x) <- runMassT (runMaybeT mx)]
