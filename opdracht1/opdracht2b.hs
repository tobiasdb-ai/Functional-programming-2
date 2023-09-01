import System.Environment (getArgs)
import Data.List (group)
import Data.Char (isDigit)

main :: IO ()
main = do
    -- Lees de command line argumenten
    args <- getArgs
    case args of
        -- Check of er precies twee argumenten zijn meegegeven
        [inputFile, outputFile] -> do
            -- Lees de inhoud van het invoerbestand
            inputContents <- readFile inputFile
            -- Decodeer de inhoud
            let decodedContents = decode inputContents
            -- Schrijf de gedecodeerde inhoud naar het uitvoerbestand
            writeFile outputFile decodedContents
            -- Print de lengte van het oorspronkelijke en gedecodeerde bestand
            putStrLn $ "Gecomprimeerde lengte: " ++ show (length inputContents)
            putStrLn $ "Ongecomprimeerde lengte: " ++ show (length decodedContents)
            putStrLn $ "factor: " ++ show (round ((fromIntegral (length inputContents) / fromIntegral (length decodedContents)) * 100)) ++ "%"
        -- Print een bericht over het correcte gebruik van het programma als dit niet het geval is
        _ -> putStrLn "Usage: rld inputFile outputFile"

-- Run-length decoding functie
decode :: String -> String
decode [] = []
decode cs
    | isDigit (head cs) = replicate (read (takeWhile isDigit cs)) (head (dropWhile isDigit cs)) ++ decode (drop 1 (dropWhile isDigit cs))
    | otherwise = head cs : decode (tail cs)
