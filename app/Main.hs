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

pretty o mx = "P(.|"++ show o ++"): "++ concat [show x ++" = "++ show (getSum
  n) ++", " | Mass n (Just x) <- runMassT (runMaybeT mx)]

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

