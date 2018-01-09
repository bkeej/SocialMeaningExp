module LF where


data LF vocab
  = Null
  | Name String
  | Lex vocab
  | Var Int
  | App (LF vocab) (LF vocab)
  | Lam Int (LF vocab)
  deriving (Eq, Show)

app :: LF l -> LF l -> LF l
app = App

lam :: (LF l -> LF l) -> LF l
lam f = Lam n body
  where body = f (Var n)
        n = maxBV body + 1
        maxBV m = case m of
          App f x -> maxBV f `max` maxBV x
          Lam i _ -> i
          _       -> 0

