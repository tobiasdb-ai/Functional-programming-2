import System.Environment (getArgs)
import Data.List (sort)
import Control.Exception (catch, IOException)

main :: IO ()
main = do
  -- Lees de command line argumenten
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      -- Lees de inhoud van het invoerbestand
      inputContents <- readFile inputFile
      -- Sorteer de letters
      let sortedContents = sort inputContents
      -- Schrijf de gesorteerde inhoud naar het uitvoerbestand
      catch (writeFile outputFile sortedContents)
            (\e -> putStrLn $ "Error writing to file: " ++ show (e :: IOException))
    _ -> putStrLn "Usage: sortfile inputFile outputFile"
