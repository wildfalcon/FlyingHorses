module CashBook where

import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  input <- readFile "CashBook.csv"
  let
    rows = parseCSV input
    entries = map makeEntry (tail rows)
  putStrLn . show $ rows

csv :: ReadP [[String]]
csv = sepBy csvLine (char '\n')

csvLine :: ReadP [String]
csvLine = sepBy csvField (char ';' +++ char ',')

csvField :: ReadP String
csvField = quotedField +++ munch (not . (`elem` ";,"))

quotedField :: ReadP String
quotedField = between (char '"') (char '"') (munch (/= '"'))

parseCSV :: String -> [[String]]
parseCSV s = head [p | (p, "") <- readP_to_S csv s]

