import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let numbers = map parseLine (lines input)
  print $ sum numbers

parseLine :: String -> Int
parseLine s = case (getFirstDigit s, getLastDigit s) of
  (Just first, Just last) -> read (first : [last]) :: Int
  _ -> 0

getFirstDigit :: String -> Maybe Char
getFirstDigit string = case getFirstDigitAndPosition string 0 of
  Just (character, position) -> case findFirstWrittenDigit $ take position string of
    Just c -> Just c
    _ -> Just character
  _ -> Nothing

getLastDigit :: String -> Maybe Char
getLastDigit string = case getFirstDigitAndPosition (reverse string) 0 of
  Just (character, position) -> case findLastWrittenDigit $ drop (length string - position) string of
    Just c -> Just c
    _ -> Just character
  _ -> Nothing

getFirstDigitAndPosition :: String -> Int -> Maybe (Char, Int)
getFirstDigitAndPosition [] _ = Nothing
getFirstDigitAndPosition (c : cs) position =
  if isDigit c
    then Just (c, position)
    else getFirstDigitAndPosition cs (position + 1)

digitStrings :: [(String, Char)]
digitStrings = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

findFirstWrittenDigit :: String -> Maybe Char
findFirstWrittenDigit [] = Nothing
findFirstWrittenDigit string = case filter fst $ map (\(s, c) -> (s `isPrefixOf` string, c)) digitStrings of
  [] -> findFirstWrittenDigit $ tail string
  [(_, c)] -> Just c

findLastWrittenDigit :: String -> Maybe Char
findLastWrittenDigit [] = Nothing
findLastWrittenDigit string = case filter fst $ map (\(s, c) -> (s `isSuffixOf` string, c)) digitStrings of
  [] -> findLastWrittenDigit $ init string
  [(_, c)] -> Just c
