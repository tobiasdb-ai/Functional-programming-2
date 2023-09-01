module Opdracht2 where

data Bintree a
  = Empty
  | Node a (Bintree a) (Bintree a)
  deriving (Show, Eq)

push :: (Ord a) => Bintree a -> a -> Bintree a
push Empty x = Node x Empty Empty
push (Node val left right) x
  | x < val   = Node val (push left x) right
  | otherwise = Node val left (push right x)

pushlist :: (Ord a) => Bintree a -> [a] -> Bintree a
pushlist tree [] = tree  -- Basisgeval: als de lijst leeg is, retourneer de oorspronkelijke boom
pushlist tree (x:xs) = pushlist (push tree x) xs

maptree :: (a -> b) -> Bintree a -> Bintree b
maptree _ Empty = Empty  -- Basisgeval: lege boom
maptree f (Node val left right) = Node (f val) (maptree f left) (maptree f right)

filtertree :: (a -> Bool) -> Bintree a -> [a]
filtertree _ Empty = []  -- Basisgeval: lege boom, retourneer een lege lijst
filtertree f (Node val left right)
  | f val = val : rest  -- Als de voorwaarde waar is, voeg de waarde toe aan de lijst
  | otherwise = rest    -- Anders, voeg niets toe aan de lijst
  where
    rest = filtertree f left ++ filtertree f right  -- Recursief filteren in linker- en rechteronderboom

preorder :: Bintree a -> [a]
preorder Empty = []  -- Basisgeval: lege boom, retourneer een lege lijst
preorder (Node val left right) = val : preorder left ++ preorder right

postorder :: Bintree a -> [a]
postorder Empty = []  -- Basisgeval: lege boom, retourneer een lege lijst
postorder (Node val left right) = postorder left ++ postorder right ++ [val]

inorder :: Bintree a -> [a]
inorder Empty = []  -- Basisgeval: lege boom, retourneer een lege lijst
inorder (Node val left right) = inorder left ++ [val] ++ inorder right


