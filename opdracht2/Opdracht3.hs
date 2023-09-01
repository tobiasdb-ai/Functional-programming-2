import Data.Char (isDigit, chr)
import Data.Typeable (typeOf)
import Graphics.Win32 (newTString)

main :: IO ()
main = do
  -- Lees bestand uit en sla op in namesAndAges
  namesAndAges <- readFile "names.txt"
  
  -- Zet alle cijfers en letters op de tree
  let boom = pushlist Empty namesAndAges
  -- printTree boom

  -- Zet alle cijfers en letters om naar Int
  let intBoom = maptree fromEnum boom
  -- printTree intBoom

  -- Schrijf de boom naar het bestand "tree.txt"
  writeFile "tree.txt" (show intBoom)

  -- Lees de boom uit het bestand "tree.txt"
  treeContents <- readFile "tree.txt"
  let newTree = read treeContents :: Bintree Int
  -- printTree newTree

  -- Zet alle waarden om naar chr
  let charBoom = maptree chr newTree
  printTree charBoom

  -- Print de inorder string van de boom van Chars
  let inordstring = inorder charBoom
  putStrLn inordstring

  -- Print alleen de getallen in de boom van Chars
  let numberList = filtertree isDigit charBoom
  putStrLn numberList



data Bintree a
  = Empty
  | Node a (Bintree a) (Bintree a)
  deriving (Show, Eq, Read)

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

printTree :: Show a => Bintree a -> IO ()
printTree = print

