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

--
-- Parameters
--

worldPrior :: Dist m => m World
worldPrior = uniform universe

messagePrior :: Dist m => m (LF ASVocab)
messagePrior = uniform testMessages

interpPrior :: Dist m => m (Lexicon ASVocab)
interpPrior = uniform testIs

cost :: LF ASVocab -> Sum Float
cost Null = 5   -- Only null messages incur costs
cost _    = 0

pretty o mx = "P(.|"++ show o ++"): "++ concat [show x ++" = "++ show (getSum
  n) ++", " | Mass n (Just x) <- runMassT (runMaybeT mx)]

disp_s n = sequence_ (map print test)
  where test = [pretty w (speaker n w testI1 cost worldPrior messagePrior)   | w <- universe]

disp_l n = sequence_ (map print test)
  where test = [pretty m (listener n m testI1 cost worldPrior messagePrior)  | m <- testMessages]

disp_L n = sequence_ (map print test)
  where test = [pretty m (lexicaListener n m cost worldPrior messagePrior interpPrior) | m <- testMessages]

testMessages :: [LF ASVocab]
testMessages =
  [ Lex Aced `app` Name "John"
  , Lex Scored `app` Name "John"
  , Null
  ]
