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

module LexicalUncertainty where

import           Control.Monad             (guard)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Data.Function             (on)
import           Data.List                 (partition)
import           Data.Maybe                (fromJust)
import           Lexica
import           LF
import           ModelTheory
import           Prob
import           Utils

type Env = [(Int, Value)]

eval :: Lexicon vocab -> Env -> (LF vocab) -> Value
eval sem env term = case term of
  Null      -> VF (\w -> VT True)
  Name e    -> VE (case e of {"John" -> John ; "Mary" -> Mary})
  Lex l     -> sem l
  Var n     -> fromJust $ lookup n env
  Lam n mb  -> VF (\x -> eval sem ((n,x):env) mb)
  App mf mx -> (eval sem env mf) @@ (eval sem env mx)

--
-- A model includes a base lexicon, priors over worlds/messages/lexica, and
-- specifications of cost and temperature parameters.
--

data ParamModel m l = PM
  { lexicon      :: Lexicon l
  , worldPrior   :: m World
  , messagePrior :: m (LF l)
  , lexiconPrior :: m (Lexicon l)
  , cost         :: LF l -> Sum Float
  , temp         :: Sum Float
  }

--
-- Arbitrarily pragmatic agents given by mutually recursive functions.
-- The definitions here are somewhat more general than those of Potts et al.
--

speaker :: Eq l => Int -> World -> ParamModel BDDist l -> BDDist (LF l)
speaker n w model = bayes $ do
  m  <- messagePrior model
  w' <- scale m model (listener (n-1) m model)
  guard (w' == w)
  return m

listener :: (Eq l) => Int -> LF l -> ParamModel BDDist l -> BDDist World
listener n m model = bayes $ do
  w <- worldPrior model
  if n <= 0   -- literal listener
    then case (eval (lexicon model) [] m) @@ (VS w) of VT t -> guard t
    else do   -- pragmatic listener
      m' <- speaker n w model
      guard (m' == m)
  return w

-- Helper functions for scaling probabilities
scale :: LF l -> ParamModel m l -> BDDist a -> BDDist a
scale m model = modify (exp . (temp model *) . subtract (cost model m) . log)

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

--
-- Variable-lexica agents
--

lexicaSpeaker :: Eq l => Int -> World -> ParamModel BDDist l -> BDDist (LF l)
lexicaSpeaker n w model = bayes $ do
  m  <- messagePrior model
  w' <- scale m model (lexicaListener (n-1) m model)
  guard (w' == w)
  return m

lexicaListener :: (Eq l) => Int -> LF l -> ParamModel BDDist l -> BDDist World
lexicaListener n m model = weightedEq . runMassT . bayes $ do
  w  <- worldPrior model
  m' <- if n <= 1
          then do
            sem <- lexiconPrior model
            speaker n w (model {lexicon = sem})
          else lexicaSpeaker n w model
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
