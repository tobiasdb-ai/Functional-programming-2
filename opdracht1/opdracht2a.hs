import System.Environment (getArgs)
import Data.List (group)

main :: IO ()
main = do
    -- Lees de command line argumenten
    args <- getArgs
    case args of
        -- Check of er precies twee argumenten zijn meegegeven
        [inputFile, outputFile] -> do
            -- Lees de inhoud van het invoerbestand
            inputContents <- readFile inputFile
            -- Voer run-length encoding uit op de inhoud
            let encodedContents = encode inputContents
            -- Schrijf de gecomprimeerde inhoud naar het uitvoerbestand
            writeFile outputFile encodedContents
            -- Print de lengte van het oorspronkelijke en gecomprimeerde bestand
            putStrLn $ "Ongecomprimeerde lengte: " ++ show (length inputContents)
            putStrLn $ "Gecomprimeerde lengte: " ++ show (length encodedContents)
            putStrLn $ "factor: " ++ show (round ((fromIntegral (length encodedContents) / fromIntegral (length inputContents)) * 100)) ++ "%"
        -- Print een bericht over het correcte gebruik van het programma als dit niet het geval is
        _ -> putStrLn "Usage: rle inputFile outputFile"

-- Run-length encoding functie
encode :: String -> String
encode s = concatMap encodeGroup (group s)
  where 
    -- Encodeer een groep tekens
    encodeGroup :: String -> String
    encodeGroup g
      -- Als de groep uit één teken bestaat, geef dat teken ongecodeerd terug
      | length g == 1 = g
      -- Als de groep uit meerdere tekens bestaat, geef de lengte van de groep en het eerste teken terug
      | otherwise = (show $ length g) ++ [head g]
