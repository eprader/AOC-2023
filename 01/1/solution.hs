import Data.Char ( isDigit)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let numbers = map parseLine (lines input)
  print numbers
  print $ sum numbers

parseLine :: String -> Int
parseLine s = case (getFirstDigit s, getLastDigit s) of
  (Just first, Just last) -> read (first : [last]) :: Int
  _ -> 0

getFirstDigit :: String -> Maybe Char
getFirstDigit [] = Nothing
getFirstDigit (c : cs) = if isDigit c then Just c else getFirstDigit cs

getLastDigit :: String -> Maybe Char
getLastDigit string = getFirstDigit $ reverse string

-- NOTE: The solution is 53334
