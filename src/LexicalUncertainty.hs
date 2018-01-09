{-
 - The model defined below is intended to be equivalent to that of Potts et
 - al. 2015[^1] (specifically, the extended model in their Appendix A, of
 - which the model in the main text is a special case).
 -
 - Running `disp_s 1` reproduces table s_1 in the left-most column of their
 - Figure 2 (page 773), and running `disp_L 1` reproduces table L at the top
 - of their Figure 2 (modulo rounding).
 -
 - [^1]: https://doi.org/10.1093/jos/ffv012
-}

module LexicalUncertainty where

import Control.Monad             (guard)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe                (fromJust)
import Data.List                 (partition)
import Data.Function             (on)
import Prob
import Utils
import ModelTheory
import LF
import Lexica

type Env = [(Int, Value)]

eval :: Lexicon vocab -> Env -> (LF vocab) -> Value
eval sem env term = case term of
  Null -> VF (\w -> VT True)
  Name e -> VE (case e of {"John" -> John ; "Mary" -> Mary})
  Lex l -> sem l
  Var n -> fromJust $ lookup n env
  Lam n mb -> VF (\x -> eval sem ((n,x):env) mb)
  App mf mx -> (eval sem env mf) @@ (eval sem env mx)



--
-- Arbitrarily pragmatic agents given by mutually recursive functions.
-- The definitions here are somewhat more general than that of Potts et al.
--

listener :: (Eq l) => Int -> LF l -> Lexicon l -> (LF l -> Sum Float) -> BDDist World -> BDDist (LF l) -> BDDist World
listener n m sem cost wrldPrior msgPrior = bayes $ do
  w <- wrldPrior
  if n <= 0   -- literal listener
    then case (eval sem [] m) @@ (VS w) of VT t -> guard t
    else do   -- pragmatic listener
      m' <- speaker n w sem cost wrldPrior msgPrior
      guard (m' == m)
  return w

speaker :: Eq l => Int -> World -> Lexicon l -> (LF l -> Sum Float) -> BDDist World -> BDDist (LF l) -> BDDist (LF l)
speaker n w sem cost wrldPrior msgPrior = bayes $ do
  m  <- msgPrior
  w' <- scaleProb m cost (listener (n-1) m sem cost wrldPrior msgPrior)
  guard (w' == w)
  return m

-- Helper functions for scaling probabilities
scaleProb :: LF l -> (LF l -> Sum Float) -> BDDist a -> BDDist a
scaleProb m cost = modify (exp . (temperature *) . subtract (cost m) . log)

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

temperature = 1


--
-- Variable-lexica agents
--


lexicaListener :: Eq l => Int -> LF l -> (LF l -> Sum Float) -> BDDist World -> BDDist (LF l) -> BDDist (Lexicon l) -> BDDist World
lexicaListener n m cost wrldPrior msgPrior intPrior = weightedEq . runMassT . bayes $ do
 w <- wrldPrior
 if n <= 1
   then do
     sem <- intPrior
     m'  <- speaker n w sem cost wrldPrior msgPrior
     guard (m' == m)
   else do
     m'  <- lexicaSpeaker n w cost wrldPrior msgPrior intPrior
     guard (m' == m)
 return w

lexicaSpeaker :: Eq l => Int -> World -> (LF l -> Sum Float) -> BDDist World -> BDDist (LF l) -> BDDist (Lexicon l) -> BDDist (LF l)
lexicaSpeaker n w cost wrldPrior msgPrior intPrior = bayes $ do
 m  <- msgPrior
 w' <- scaleProb m cost (lexicaListener (n-1) m cost wrldPrior msgPrior intPrior)
 guard (w' == w)
 return m

-- Helper function for summing probabilities of identical outcomes
weightedEq :: (Dist m, Eq a) => [Mass Prob a] -> m a
weightedEq vs = weighted (concatMap col bins)
  where -- vsOrd = sortBy  (\x y -> compare (getSndMass x) (getSndMass y)) vs
        -- bins  = groupBy (\x y -> (==)    (getSndMass x) (getSndMass y)) vsOrd
        bins = groupEqBy ((==) `on` getSndMass) vs
        col []                = []
        col ms@((Mass _ x):_) = [Mass (sum (map getFstMass ms)) x]

groupEqBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupEqBy _ [] = []
groupEqBy f (a:rest) = (a:as) : groupEqBy f bs
  where (as,bs) = partition (f a) rest
