module Lexica where

import ModelTheory
import LF

data ASVocab
  = Some
  | All
  | Aced
  | Scored
  deriving (Eq, Show)

type Lexicon vocab = vocab -> Value

testI1 :: Lexicon ASVocab
testI1 s = case s of
  Aced -> VF (\x -> VF (\(VS w) -> aced' w @@ x))
  Scored -> VF (\x -> VF (\(VS w) -> scored' w @@ x))

testI2 :: Lexicon ASVocab
testI2 s = case s of
  Aced -> VF $ \x -> VF $ \(VS w) -> aced' w @@ x
  Scored -> VF $ \x -> VF $ \(VS w) -> (scored' w `cap` (neg $ aced' w)) @@ x

testI3 :: Lexicon ASVocab
testI3 s = case s of
  Aced -> VF (\x -> VF (\(VS w) -> aced' w @@ x))
  Scored -> VF (\x -> VF (\(VS w) -> aced' w @@ x))

testIs ::[Lexicon ASVocab]
testIs = [testI1, testI2, testI3]
