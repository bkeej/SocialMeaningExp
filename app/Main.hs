module Main where

import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Lexica
import           LexicalUncertainty
import           LF
import           ModelTheory
import           Prob
import           Utils

main :: IO ()
main = disp_s 1 >> putStrLn "" >> disp_L 1

--
-- Parameters
--

asModel :: Dist m => ParamModel m ASVocab
asModel = PM
  { lexicon      = asLex1
  , worldPrior   = uniform asUniverse
  , messagePrior = uniform asMessages
  , lexiconPrior = uniform asLexica
  , cost         = \x -> if x == Null then 5 else 0
  , temp         = 1
  }

--
-- Testing the model
--

pretty :: (Show a, Show b) => a -> BDDist b -> String
pretty o mx = dropR 2 probs where
  probs = "P(.|" ++ show o ++"): " ++ concatMap f (runMassT (runMaybeT mx))
  f = \(Mass n (Just x)) -> show x ++ " = " ++ prettyN n ++ ", "
  prettyN (Sum n) = show (fromInteger (round (n * 100)) / 10.0^^2)
  dropR n xs = fst (splitAt (length xs - n) xs)

disp_s n = sequence_ (map putStrLn test)
  where test = [pretty w (speaker n w asModel) | w <- asUniverse]

disp_l n = sequence_ (map putStrLn test)
  where test = [pretty m (listener n m asModel) | m <- asMessages]

disp_L n = sequence_ (map putStrLn test)
  where test = [pretty m (lexicaListener n m asModel) | m <- asMessages]
