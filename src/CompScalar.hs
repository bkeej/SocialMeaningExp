{-
 - RSA model for scalar implicature, with lexical uncertainty and null-message
 - costs (and an idle but fiddle-with-able temperature parameter).
 -
 - The model defined below is intended to be equivalent to that of Potts et
 - al. 2015[^1] (specifically, the extended model in their Appendix A, of
 - which the model in the main text is a special case).
 -
 - Running `disp_s 1` reproduces table s_1 in the left-most column of their
 - Figure 2 (page 773), and running `disp_L 1` reproduces table L at the top
 - of their Figure 2 (modulo rounding).
 -
 - To do: Think about more principled and/or general approaches to `modify`;
 - look at local implicature generation; think about how to automatically
 - generate refined lexica; play with exceptional implicature.
 -
 - [^1]: https://doi.org/10.1093/jos/ffv012
-}

module CompScalar where

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
-- The definitions here are somewhat more general than those of Potts et al.
--

type Cost l = LF l -> Sum Float
type Agent l a = Lexicon l -> Cost l -> BDDist World -> BDDist (LF l) -> BDDist a

listener :: (Eq l) => Int -> LF l -> Agent l World
listener n m sem cost wrldPrior msgPrior = bayes $ do
  w <- wrldPrior
  if n <= 0   -- literal listener
    then case (eval sem [] m) @@ (VS w) of VT t -> guard t
    else do   -- pragmatic listener
      m' <- speaker n w sem cost wrldPrior msgPrior
      guard (m' == m)
  return w

speaker :: Eq l => Int -> World -> Agent l (LF l)
speaker n w sem cost wrldPrior msgPrior = bayes $ do
  m  <- msgPrior
  w' <- scaleProb m cost (listener (n-1) m sem cost wrldPrior msgPrior)
  guard (w' == w)
  return m

-- Helper functions for scaling probabilities
scaleProb :: LF l -> Cost l -> BDDist a -> BDDist a
scaleProb m cost = modify (exp . (temperature *) . subtract (cost m) . log)

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

temperature = 1

--
-- Variable-lexica agents
--

type VLAgent l a = Cost l -> BDDist World -> BDDist (LF l) -> BDDist (Lexicon l) -> BDDist a

lexicaSpeaker :: Eq l => Int -> World -> VLAgent l (LF l)
lexicaSpeaker n w cost wldPrior msgPrior lexPrior = bayes $ do
 m  <- msgPrior
 w' <- scaleProb m cost (lexicaListener (n-1) m cost wldPrior msgPrior lexPrior)
 guard (w' == w)
 return m

lexicaListener :: Eq l => Int -> LF l -> VLAgent l World
lexicaListener n m cost wldPrior msgPrior lexPrior = weightedEq . runMassT . bayes $ do
  w  <- wldPrior
  m' <- if n <= 1
          then do
            sem <- lexPrior
            speaker n w sem cost wldPrior msgPrior
          else lexicaSpeaker n w cost wldPrior msgPrior lexPrior
  guard (m' == m)
  return w

-- Sum weights of identical outcomes
weightedEq :: (Dist m, Eq a) => [Mass Prob a] -> m a
weightedEq vs = weighted (concatMap col bins)
  where bins = groupEqBy ((==) `on` getSndMass) vs
        col []                = []
        col ms@((Mass _ x):_) = [Mass (sum (map getFstMass ms)) x]

groupEqBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupEqBy _ [] = []
groupEqBy f (a:rest) = (a:as) : groupEqBy f bs
  where (as,bs) = partition (f a) rest
