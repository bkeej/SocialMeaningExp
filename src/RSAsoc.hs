{-
 - Implements extention of Burnett-style Social Meaning Games[^1] for dogwhistles 
 - following work by Henderson and McCready[^2]. Based on RSA.hs. Test the model by
 - giving disp_util an audience with a particular distribution of Ingroup, Savvy, and
 - Naive listeners---i.e. disp_util (audience 5 6 1), which will return the social 
 - utility of each message. Compare to disp_util (audience 5 1 6) to see the dogwhistle
 - effect, that is, the effect of swapping naive for savvy outgroup audience members.
 -
 - [^1]: https://www.rhenderson.net/resources/papers/how_dogwhistles_work.pdf
 - [^2]: http://www.heatherburnett.net/uploads/9/6/6/0/96608942/jofs_burnett.pdf
-}

module RSAsoc where

import           Control.Monad             (guard)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Prob
import           Utils
import           Data.List                 (nub)
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
eval m f = Data.List.nub $ [i | i <- f, 
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
deno CorpSci = [ProVax, AntiCorp]

--
-- Model parameters
--

-- Across-the-board no-cost messages
cost _ = 0

-- Higher values ~> more eager pragmatic reasoning
temperature = 1 

personaPrior :: Dist m => Group -> m Persona
personaPrior g = weighted [Mass 5 [ProVax,ProCorp], Mass 40 [ProVax,AntiCorp], Mass 15 [AntiVax,ProCorp], Mass 40 [AntiVax,AntiCorp]]
-- personaPrior Ingroup = uniform field
-- personaPrior Savvy = uniform field
-- personaPrior Naive = uniform field
-- personaPrior Uniform = uniform field

messagePrior :: Dist m => Group -> Persona -> m Message
messagePrior Ingroup [AntiVax, AntiCorp] = weighted [Mass 80 BigPharma, Mass 10 CorpSci]
messagePrior Ingroup [AntiVax, ProCorp] = weighted [Mass 10 BigPharma, Mass 10 CorpSci]
messagePrior Ingroup [ProVax, AntiCorp] = weighted [Mass 10 BigPharma, Mass 80 CorpSci]
messagePrior Ingroup [ProVax, ProCorp] = weighted [Mass 0 BigPharma, Mass 60 CorpSci]

messagePrior Savvy [AntiVax, AntiCorp] = weighted [Mass 80 BigPharma, Mass 10 CorpSci]
messagePrior Savvy [AntiVax, ProCorp] = weighted [Mass 10 BigPharma, Mass 10 CorpSci]
messagePrior Savvy [ProVax, AntiCorp] = weighted [Mass 10 BigPharma, Mass 80 CorpSci]
messagePrior Savvy [ProVax, ProCorp] = weighted [Mass 0 BigPharma, Mass 60 CorpSci]

messagePrior Naive [AntiVax, AntiCorp] = weighted [Mass 15 BigPharma, Mass 10 CorpSci]
messagePrior Naive [AntiVax, ProCorp] = weighted [Mass 10 BigPharma, Mass 10 CorpSci]
messagePrior Naive [ProVax, AntiCorp] = weighted [Mass 70 BigPharma, Mass 80 CorpSci]
messagePrior Naive [ProVax, ProCorp] = weighted [Mass 0 BigPharma, Mass 60 CorpSci]

messagePrior Uniform x = uniform [BigPharma ..]



-- messagePrior Uniform [AntiVax, AntiCorp] = weighted [Mass 10 BigPharma, Mass 90 CorpSci]

--
-- Mutually recursive pragmatic reasoning
--

speaker :: Int -> Group -> Persona -> Lexicon -> BDDist Message
speaker n g p sem = bayes $ do
  m <- messagePrior g p
  scaleProb m $ if n <= 0   -- literal speaker
                  then guard (p `elem` sem m field)
                  else do   -- pragmatic speaker
                    p' <- listener n g m sem
                    guard (p' == p)
  return m

listener :: Int -> Group -> Message -> Lexicon -> BDDist Persona
listener n g m sem = bayes $ do
  p  <- personaPrior g
  m' <- speaker (n-1) g p sem
  guard (m' == m)
  return p

-- Helper functions for scaling probabilities
scaleProb :: Message -> BDDist a -> BDDist a
scaleProb m = modify (exp . (temperature *) . subtract (cost m) . log)

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

--
-- Social Utility Calculations
--

-- Affective values, here initialized with 0

-- Speaker is a politician, i.e., no preference for persona, only want to maximize listener approval.
vS :: Persona -> Float
vS [ProVax,ProCorp] = 0
vS [ProVax,AntiCorp] = 0
vS [AntiVax,ProCorp] = 0
vS [AntiVax,AntiCorp] = 0


vL :: Group -> Persona -> Float
vL Ingroup [ProVax,ProCorp] = -100
vL Ingroup [ProVax,AntiCorp] = -100
vL Ingroup [AntiVax,ProCorp] = 100
vL Ingroup [AntiVax,AntiCorp] = 100

-- Naive and Savvy audiences punish anti-vaxers and prefer anti-corporate ones
vL Naive [ProVax,ProCorp] = 75
vL Naive [ProVax,AntiCorp] = 100
vL Naive [AntiVax,ProCorp] = -100
vL Naive [AntiVax,AntiCorp] = -100

vL Savvy x = vL Naive x

-- Social utility calculation
uSoc :: Message -> Persona -> Group -> Lexicon -> Float
uSoc m p g l = pr + (vL g p * pr) + (vS p * pr)
  where Sum pr = sum $ [x | Mass x (Just y) <- runMassT (runMaybeT (RSAsoc.listener 1 g m eval)), y == p]


-- Social utility for a message suming over all personas
uSocM :: Message -> Group -> Lexicon -> Float
uSocM m g l = sum $ map (\p -> uSoc m p g l) field

-- Audiences
type Audience = [Group]

-- Social utility expecting an audience argument
uASoc :: Audience -> Message -> Persona -> Lexicon -> Float
uASoc a m p l = sum $ map (\g -> uSoc m p g l) a 

-- Social utility for a message suming over all personas expecting an audience argument
uASocM :: Audience -> Message -> Lexicon -> Float
uASocM a m l = sum $ map (\g -> uSocM m g l) a 

-- Structured Audiences

audience :: Int -> Int -> Int -> Audience 
audience x y z = take x (repeat Ingroup) ++ take y (repeat Savvy) ++ take z (repeat Naive)

--
-- Testing the model
--

disp_util a = sequence_ (map print test)
  where test = [prettyUtils m (uASocM a m eval) | m <- [BigPharma ..]]   

prettyUtils m x = "U("++ show m ++")=" ++ show x 

disp_s n g = sequence_ (map print test)
  where test = [pretty w (speaker n g w eval)   | w <- field]

disp_l n g = sequence_ (map print test)
  where test = [pretty m (listener n g m eval)  | m <- [BigPharma ..]]

pretty o mx = "P(.|"++ show o ++"): "++ concat [show x ++" = "++ show (getSum
  n) ++", " | Mass n (Just x) <- runMassT (runMaybeT mx)]
