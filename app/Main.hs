module Main where

import Data.Maybe (fromJust)
import Lexica
import ModelTheory
import LF
import LexicalUncertainty
import Prob
import Utils
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))

main :: IO ()
main = disp_s 1 >> putStrLn "" >> disp_L 1
-- main = return ()

--
-- Parameters
--

data ParamModel m l = PM
  { worldPrior :: m World
  , messagePrior :: m (LF l)
  , lexiconPrior :: m (Lexicon l)
  }

asModel :: Dist m => ParamModel m ASVocab
asModel = PM
  { worldPrior = uniform asUniverse
  , messagePrior = uniform asMessages
  , lexiconPrior = uniform asLexica
  }

asCost :: Cost ASVocab
asCost Null = 5   -- Only null messages incur asCosts
asCost _    = 0

--
-- Testing the model
--

pretty o mx = dropR 2 probs where
  probs = "P(.|" ++ show o ++"): " ++ concatMap f (runMassT (runMaybeT mx))
  f = \(Mass n (Just x)) -> show x ++ " = " ++ prettyN n ++ ", "
  prettyN (Sum n) = show (fromInteger (round (n * 100)) / 10.0^^2)
  dropR n xs = fst (splitAt (length xs - n) xs)

disp_s n = sequence_ (map print test)
  where test = [pretty w (speaker n w asLex1 asCost wp mp)   | w <- asUniverse]
        wp = worldPrior asModel
        mp = messagePrior asModel

disp_l n = sequence_ (map print test)
  where test = [pretty m (listener n m asLex1 asCost wp mp)  | m <- asMessages]
        wp = worldPrior asModel
        mp = messagePrior asModel

disp_L n = sequence_ (map print test)
  where test = [pretty m (lexicaListener n m asCost wp mp lp) | m <- asMessages]
        wp = worldPrior asModel
        mp = messagePrior asModel
        lp = lexiconPrior asModel

