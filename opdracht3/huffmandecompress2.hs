module Main where

import System.Environment

data Codetree a = Branch Int (Codetree a) (Codetree a)
                | Branch2 Int Char
                deriving (Show, Eq, Ord, Read)

-- hulp functie om bits van volgend karakter te pakken
split :: Codetree a -> [Char] -> (Char, [Char])
split (Branch2 _ chr) bits = (chr, bits)
split (Branch _ tree1 tree2) bits
  | head bits == '1' = split tree1 rest
  | otherwise = split tree2 rest
  where rest = drop 1 bits

-- functie om te decompressen: boom & bits naar string (recursief)
decompress :: Codetree a -> [Char] -> [Char]
decompress _ [] = []
decompress tree bitString = fst tuple : decompress tree (snd tuple)
  where tuple = split tree bitString

main = do [sourcefile, targetfile, codetreefile] <- getArgs
          filecontent <- readFile sourcefile

          strCodetree <- readFile codetreefile
          let codetree = (read strCodetree :: Codetree a)

          let uncompressedContent = decompress codetree filecontent

          let lenUncompressed = length uncompressedContent

          putStrLn $ "length of decompressed file: " ++ show lenUncompressed ++ " characters, " ++ show (lenUncompressed  * 8) ++ " bits."
          writeFile targetfile uncompressedContent
          putStrLn $ targetfile ++ " written to disk..."
          putStrLn "done"

-- runhaskell huffmandecompress2.hs coded.txt decoded.txt tree.txt