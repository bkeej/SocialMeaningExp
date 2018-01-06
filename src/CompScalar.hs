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

module CompScalar where

import           Control.Monad             (guard)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Data.List                 (partition, groupBy, sortBy)
import           Data.Function             (on)
import           Data.Maybe                (fromJust)
import           Prob
import           Utils

--
-- Model-theoretic stuff
--

data Value
  = VE String
  | VT Bool
  | VS World
  | VF (Value -> Value)

instance Show Value where
  show (VF f) = "<<fun>>"
  show (VS w) = "<<world>>"
  show (VE e) = show e
  show (VT t) = show t

data World = World
  { aced' :: Value
  , scored' :: Value
  }

instance Eq World where
  World (VF ua) (VF us) == World (VF va) (VF vs) =
    let (VT uaj, VT vaj) = (ua (VE "John"), va (VE "John"))
        (VT uam, VT vam) = (ua (VE "Mary"), va (VE "Mary"))
        (VT usj, VT vsj) = (us (VE "John"), vs (VE "John"))
        (VT usm, VT vsm) = (us (VE "Mary"), vs (VE "Mary"))
    in uaj == vaj && uam == vam && usj == vsj && usm == vsm

instance Show World where
  show w | w == wA = "wA"
         | w == wS = "wS"
         | w == wN = "wN"

(@@) :: Value -> Value -> Value
(VF f) @@ x = f x

cup :: Value -> Value -> Value
cup (VT p) (VT q) = VT (p || q)
cup (VF f) (VF g) = VF (\x -> f x `cup` g x)

cap :: Value -> Value -> Value
cap (VT p) (VT q) = VT (p && q)
cap (VF f) (VF g) = VF (\x -> f x `cap` g x)

neg :: Value -> Value
neg (VT t) = VT (not t)
neg (VF f) = VF (\x -> neg (f x))

wA, wS, wN :: World
wA = World
  { aced' = VF (\(VE x) -> VT (x == "John"))
  , scored' = VF (\(VE x) -> VT (x == "John"))
  }
wS = World
  { aced' = VF (\(VE x) -> VT False)
  , scored' = VF (\(VE x) -> VT (x == "John"))
  }
wN = World
  { aced' = VF (\(VE x) -> VT False)
  , scored' = VF (\(VE x) -> VT False)
  }

universe :: [World]
universe = [wA, wS, wN]

--
-- Language stuff
--

data Mess l
  = Null
  | Name String
  | Lex l
  | Var Int
  | App (Mess l) (Mess l)
  | Lam Int (Mess l)
  deriving (Eq, Show)

data TestLexicon
  = Some
  | All
  | Aced
  | Scored
  deriving (Eq, Show)

type Env = [(Int, Value)]

eval :: Interp l -> Env -> (Mess l) -> Value
eval sem env term = case term of
  Null -> VF (\w -> VT True)
  Name e -> VE e
  Lex l -> sem l
  Var n -> fromJust $ lookup n env
  Lam n mb -> VF (\x -> eval sem ((n,x):env) mb)
  App mf mx -> (eval sem env mf) @@ (eval sem env mx)

app :: Mess l -> Mess l -> Mess l
app = App

lam :: (Mess l -> Mess l) -> Mess l
lam f = Lam n body
  where body = f (Var n)
        n = maxBV body + 1
        maxBV m = case m of
          App f x -> maxBV f `max` maxBV x
          Lam i _ -> i
          _       -> 0

type Interp l = l -> Value

testI1 :: Interp TestLexicon
testI1 s = case s of
  Aced -> VF (\x -> VF (\(VS w) -> aced' w @@ x))
  Scored -> VF (\x -> VF (\(VS w) -> scored' w @@ x))

testI2 :: Interp TestLexicon
testI2 s = case s of
  Aced -> VF $ \x -> VF $ \(VS w) -> aced' w @@ x
  Scored -> VF $ \x -> VF $ \(VS w) -> (scored' w `cap` (neg $ aced' w)) @@ x

testI3 :: Interp TestLexicon
testI3 s = case s of
  Aced -> VF (\x -> VF (\(VS w) -> aced' w @@ x))
  Scored -> VF (\x -> VF (\(VS w) -> aced' w @@ x))

testIs ::[Interp TestLexicon]
testIs = [testI1, testI2, testI3]

testMessages :: [Mess TestLexicon]
testMessages =
  [ Lex Aced `app` Name "John"
  , Lex Scored `app` Name "John"
  , Null
  ]

--
-- Parameters
--

worldPrior :: Dist m => m World
worldPrior = uniform universe     -- The primitive notion in Potts et al. is a
                                  -- prior over *sets* of worlds/states (i.e.,
                                  -- propositions).

messagePrior :: Dist m => m (Mess TestLexicon)
messagePrior = uniform testMessages  -- No correspondent in Potts et al. Flat
                                     -- here so makes no difference.

interpPrior :: Dist m => m (Interp TestLexicon)
interpPrior = uniform testIs          -- Like worlds, the primitive thing in Potts
                                      -- et al. is a prior over *sets* of lexica.

cost Null = 5   -- Only null messages incur costs
cost _    = 0   -- Coerced into Sum's by GHC?

--
-- Arbitrarily pragmatic agents given by mutually recursive functions.
-- The definitions here are somewhat more general than that of Potts et al.
--

listener :: (Eq l) => Int -> Mess l -> Interp l -> BDDist (Mess l) -> BDDist World
listener n m sem msgPrior = bayes $ do
  w <- worldPrior
  if n <= 0   -- literal listener
    then case (eval sem [] m) @@ (VS w) of VT t -> guard t
    else do   -- pragmatic listener
      m' <- speaker n w sem msgPrior
      guard (m' == m)
  return w

speaker :: Eq l => Int -> World -> Interp l -> BDDist (Mess l) -> BDDist (Mess l)
speaker n w sem msgPrior = bayes $ do
  m  <- msgPrior
  w' <- scaleProb m (listener (n-1) m sem msgPrior)
  guard (w' == w)
  return m

-- Helper functions for scaling probabilities
scaleProb :: Mess l -> BDDist a -> BDDist a
scaleProb m = modify (exp . (temperature *) . subtract (cost m) . log)

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT (runMaybeT mx)]

temperature = 1


--
-- Variable-lexica agents
--


lexicaListener :: Eq l => Int -> Mess l -> BDDist (Mess l) -> BDDist (Interp l) -> BDDist World
lexicaListener n m msgPrior intPrior = weightedEq . runMassT . bayes $ do
 w <- worldPrior
 if n <= 1
   then do
     sem <- intPrior
     m'  <- speaker n w sem msgPrior
     guard (m' == m)
   else do
     m'  <- lexicaSpeaker n w msgPrior intPrior
     guard (m' == m)
 return w

lexicaSpeaker :: Eq l => Int -> World -> BDDist (Mess l) -> BDDist (Interp l) -> BDDist (Mess l)
lexicaSpeaker n w msgPrior intPrior = bayes $ do
 m  <- msgPrior
 w' <- scaleProb m (lexicaListener (n-1) m msgPrior intPrior)
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

--
-- Testing the model
--

pretty o mx = "P(.|"++ show o ++"): "++ concat [show x ++" = "++ show (getSum
  n) ++", " | Mass n (Just x) <- runMassT (runMaybeT mx)]

disp_s n = sequence_ (map print test)
  where test = [pretty w (speaker n w testI1 messagePrior)   | w <- universe]

disp_l n = sequence_ (map print test)
  where test = [pretty m (listener n m testI1 messagePrior)  | m <- testMessages]

disp_L n = sequence_ (map print test)
  where test = [pretty m (lexicaListener n m messagePrior interpPrior) | m <- testMessages]
