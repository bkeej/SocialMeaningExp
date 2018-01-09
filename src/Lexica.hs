module Lexica where

import ModelTheory
import LF

-- 'aced', 'scored' messages
data ASVocab
  = Aced
  | Scored
  deriving (Eq, Show)

type Lexicon vocab = vocab -> Value

asLex1 :: Lexicon ASVocab
asLex1 s = case s of
  Aced -> VF (\x -> VF (\(VS w) -> aced' w @@ x))
  Scored -> VF (\x -> VF (\(VS w) -> scored' w @@ x))

asLex2 :: Lexicon ASVocab
asLex2 s = case s of
  Aced -> VF $ \x -> VF $ \(VS w) -> aced' w @@ x
  Scored -> VF $ \x -> VF $ \(VS w) -> (scored' w `cap` (neg $ aced' w)) @@ x

asLex3 :: Lexicon ASVocab
asLex3 s = case s of
  Aced -> VF (\x -> VF (\(VS w) -> aced' w @@ x))
  Scored -> VF (\x -> VF (\(VS w) -> aced' w @@ x))

asLexica ::[Lexicon ASVocab]
asLexica = [asLex1, asLex2, asLex3]

asMessages :: [LF ASVocab]
asMessages =
  [ Lex Aced `app` Name "John"
  , Lex Scored `app` Name "John"
  , Null
  ]

