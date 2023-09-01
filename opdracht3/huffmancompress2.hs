module Main where
import Data.List
import System.Environment
import Data.Functor
import Control.Applicative

-- convert string (in bestand) naar tabel
huffman1 :: String -> [(Int, Char)]
huffman1 str = reverse (sortOn fst (map (\str -> (length str, head str) ) (group (sort str))))

-- boom maken
data Codetree a = Branch Int (Codetree a) (Codetree a)
                | Branch2 Int Char
                deriving (Show, Eq, Ord, Read)

-- hulpfunctie om waarde van de boom
value :: Codetree a -> Int
value (Branch a _ _) = a
value (Branch2 a _) = a 

-- functie om tweede stap te implementeren (sorteren tabel)
voorbereidinghuffman2 :: [(Int, Char)] -> [Codetree a]
voorbereidinghuffman2 tuples = sort [Branch2 int chr | (int, chr) <- tuples]

-- bouwen coderingsboom
huffman2 :: [Codetree a] -> [Codetree a]
huffman2 (x:xs:xss) = huffman2 (sort (Branch (value x + value xs) x xs : xss))
huffman2 x = x

-- bouw een binaire tabel uit de boom
huffman3 :: Codetree a -> [Char] -> [([Char], Char)]
huffman3 (Branch2 int chr) bits = [(bits, chr)]
huffman3 (Branch int tree1 tree2) bits = huffman3 tree1 (bits++"1") ++ huffman3 tree2 (bits++"0")

-- hulp functie om elk karakter om te zetten naar bits
charToBits :: Char -> [([Char], Char)] -> [Char]
charToBits chr table = fst (head (filter condition table))
  where condition (_, tableChr) = tableChr == chr

-- karakters naar bits
huffman4 :: [([Char], Char)] -> [Char] -> [Char]
huffman4 table str = concat [charToBits chr table | chr <- str]


main = do [sourcefile, targetfile, codetreefile] <- getArgs
          filecontent <- readFile sourcefile

          -- bouwen van de boom
          let codetree = head (huffman2 (voorbereidinghuffman2 (huffman1 filecontent)))
          -- binaire tabel en omzetten naar bits
          let compressedContent = huffman4 (sortOn fst (huffman3 codetree [])) filecontent

          -- berekenen van lengte content
          let lenSource = length filecontent * 8
          let lenCompressed = length compressedContent
          let factor = round (fromIntegral lenCompressed / fromIntegral lenSource * 100)

          putStrLn $ "length of " ++ sourcefile ++ ": " ++ show (lenSource `div` 8) ++ " characters, " ++ show lenSource ++ " bits."
          putStrLn $ "length of compressed file " ++ targetfile ++ ": " ++ show lenCompressed ++ " bits."

          putStrLn $ "factor " ++ show lenCompressed ++ "/" ++ show lenSource ++ "*100=" ++ show factor ++ "%"

          writeFile targetfile compressedContent
          putStrLn $ targetfile ++ " written to disk..."

          writeFile codetreefile (show codetree)
          putStrLn $ show codetree ++ "written to disk..."

          putStrLn "done"

-- runhaskell ./huffmancompress2.hs input.txt coded.txt tree.txt