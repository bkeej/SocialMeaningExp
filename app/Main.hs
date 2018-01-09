module Main where

import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Data.Maybe                (fromJust)
import           Lexica
import           LexicalUncertainty
import           LF
import           ModelTheory
import           Prob
import           Utils

main :: IO ()
main = disp_s 1 >> putStrLn "" >> disp_L 1
-- main = return ()

--
-- Parameters
--

data ParamModel m l = PM
  { worldPrior   :: m World
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

asTemperature :: Temp
asTemperature = 1

asParams :: Params ASVocab
asParams = (asTemperature, asCost)

--
-- Testing the model
--

pretty o mx = dropR 2 probs where
  probs = "P(.|" ++ show o ++"): " ++ concatMap f (runMassT (runMaybeT mx))
  f = \(Mass n (Just x)) -> show x ++ " = " ++ prettyN n ++ ", "
  prettyN (Sum n) = show (fromInteger (round (n * 100)) / 10.0^^2)
  dropR n xs = fst (splitAt (length xs - n) xs)

disp_s n = sequence_ (map putStrLn test)
  where test = [pretty w (speaker n w asLex1 asParams wp mp)   | w <- asUniverse]
        wp = worldPrior asModel
        mp = messagePrior asModel

disp_l n = sequence_ (map putStrLn test)
  where test = [pretty m (listener n m asLex1 asParams wp mp)  | m <- asMessages]
        wp = worldPrior asModel
        mp = messagePrior asModel

disp_L n = sequence_ (map putStrLn test)
  where test = [pretty m (lexicaListener n m asParams wp mp lp) | m <- asMessages]
        wp = worldPrior asModel
        mp = messagePrior asModel
        lp = lexiconPrior asModel

