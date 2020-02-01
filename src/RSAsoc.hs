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

-- A social index is a set of features
type Index = [Feature]

-- Reserve the term Eckert-Montague Fields for maximal 
-- consistent subsets of some set of features.
type EMField = [Index]

-- Personae generates EMFields from a set of social indices
personae :: [Index] -> EMField
personae p = sequence p

-- Reserve the term Persona for members of an EMField, i.e.,
-- a maximally consistent set of features 
type Persona = [Feature]

-- Messages are not interpreted in worlds, but bear social meaning, 
-- namely they are interpreted as the set of personas they are consistent with.
type Lexicon = Message -> EMField -> [Persona]

-- Return the set of personas in the EMField consistent 
-- with message. Note the eval of a message is based on
-- its denotation, which is just a set of features.
eval :: Lexicon
eval m f = [i | i <- f, 
            p <- i, 
            p `elem` (deno m)]

--
-- Stein example denotations from Henderson & McCready 2018
--

data Feature = AntiVax | ProVax | ProCorp | AntiCorp
  deriving (Show, Eq, Enum)

indices = [[AntiVax,ProVax], [AntiCorp,ProCorp]]

field = personae indices

data Message = BigPharma | CorpSci
  deriving (Show, Eq, Enum)

data Group = Ingroup | Naive | Savvy | Uniform
  deriving (Show, Eq, Enum)

type Denotation = Message -> [Feature]

deno :: Denotation
deno BigPharma = [AntiVax, AntiCorp]
deno CorpSci = [AntiVax, AntiCorp]

--
-- Model parameters
--

-- Across-the-board no-cost messages
cost _ = 0

-- Affective values of personas for speakers and listeners
-- valueS :: Persona -> Group -> Int 
-- valueS = 

-- valueL :: Persona -> Group -> Int
-- valueL p g = 

-- Higher values ~> more eager pragmatic reasoning
temperature = 1 

personaPrior :: Dist m => m Persona
personaPrior = uniform field

messagePrior :: Dist m => Group -> Persona -> m Message
messagePrior Uniform [AntiVax, AntiCorp] = weighted [Mass 10 BigPharma, Mass 90 CorpSci]
messagePrior Uniform x = uniform [BigPharma ..]
messagePrior Ingroup x = uniform [BigPharma ..]
messagePrior Naive x = uniform [BigPharma ..]
messagePrior Savvy x = uniform [BigPharma ..]

--
-- Mutually recursive pragmatic reasoning

-- listener :: int -> Group -> Message -> Lexicon -> BDDist Persona
-- listener n g m sem = bayes $ do


speaker :: Int -> Group -> Persona -> Lexicon -> BDDist Message
speaker n g w sem = bayes $ do
  m <- messagePrior g w
  scaleProb m $ if n <= 0   -- literal speaker
                  then guard (w `elem` sem m field)
                  else do   -- pragmatic speaker
                    w' <- listener n g m sem
                    guard (w' == w)
  return m

listener :: Int -> Group -> Message -> Lexicon -> BDDist Persona
listener n g m sem = bayes $ do
  w  <- personaPrior
  m' <- speaker (n-1) g w sem
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

disp_s n g = sequence_ (map print test)
  where test = [pretty w (speaker n g w eval)   | w <- field]

disp_l n g = sequence_ (map print test)
  where test = [pretty m (listener n g m eval)  | m <- [BigPharma ..]]

pretty o mx = "P(.|"++ show o ++"): "++ concat [show x ++" = "++ show (getSum
  n) ++", " | Mass n (Just x) <- runMassT (runMaybeT mx)]
