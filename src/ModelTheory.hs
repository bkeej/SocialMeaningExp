module ModelTheory where


data Value
  = VE Entity
  | VT Bool
  | VS World
  | VF (Value -> Value)

instance Show Value where
  show (VF f) = "<<fun>>"
  show (VS w) = "<<world>>"
  show (VE e) = show e
  show (VT t) = show t

instance Eq Value where
  VE e == VE e' = e == e'
  VT b == VT b' = b == b'
  VS w == VS w' = w == w'
  VF f == VF f' = and [f (VE e) == f' (VE e) | e <- [John ..]]

data Entity = John | Mary deriving (Eq, Show, Ord, Enum)

data World = World
  { aced' :: Value
  , scored' :: Value
  , saw' :: Value
  }
  deriving (Eq)

baseWorld :: World
baseWorld = World
  { aced' = VF (const $ VT False)
  , scored' = VF (const $ VT False)
  , saw' = VF (const $ VT False)
  }

-- instance Eq World where
--   World (VF ua) (VF us) == World (VF va) (VF vs) =
--     let (VT uaj, VT vaj) = (ua (VE John), va (VE John))
--         (VT uam, VT vam) = (ua (VE Mary), va (VE Mary))
--         (VT usj, VT vsj) = (us (VE John), vs (VE John))
--         (VT usm, VT vsm) = (us (VE Mary), vs (VE Mary))
--     in uaj == vaj && uam == vam && usj == vsj && usm == vsm

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


-- 'aced', 'scored' model

wA, wS, wN :: World
wA = baseWorld
  { aced' = VF (\(VE x) -> VT (x == John))
  , scored' = VF (\(VE x) -> VT (x == John))
  }
wS = baseWorld
  { aced' = VF (\(VE x) -> VT False)
  , scored' = VF (\(VE x) -> VT (x == John))
  }
wN = baseWorld
  { aced' = VF (\(VE x) -> VT False)
  , scored' = VF (\(VE x) -> VT False)
  }

asUniverse :: [World]
asUniverse = [wA, wS, wN]
