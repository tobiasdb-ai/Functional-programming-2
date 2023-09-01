module Opdracht1 where

data Bintree a
  = Empty
  | Node a (Bintree a) (Bintree a)
  deriving (Show, Eq)
