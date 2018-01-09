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

data Entity = John | Mary deriving (Eq, Show, Ord)

data World = World
  { aced' :: Value
  , scored' :: Value
  }

instance Eq World where
  World (VF ua) (VF us) == World (VF va) (VF vs) =
    let (VT uaj, VT vaj) = (ua (VE John), va (VE John))
        (VT uam, VT vam) = (ua (VE Mary), va (VE Mary))
        (VT usj, VT vsj) = (us (VE John), vs (VE John))
        (VT usm, VT vsm) = (us (VE Mary), vs (VE Mary))
    in uaj == vaj && uam == vam && usj == vsj && usm == vsm

instance Show World where
  show w | w == wA = "wA"
         | w == wS = "wS"
         | w == wN = "wN"

wA, wS, wN :: World
wA = World
  { aced' = VF (\(VE x) -> VT (x == John))
  , scored' = VF (\(VE x) -> VT (x == John))
  }
wS = World
  { aced' = VF (\(VE x) -> VT False)
  , scored' = VF (\(VE x) -> VT (x == John))
  }
wN = World
  { aced' = VF (\(VE x) -> VT False)
  , scored' = VF (\(VE x) -> VT False)
  }

universe :: [World]
universe = [wA, wS, wN]


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
