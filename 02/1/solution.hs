import Data.Char (isDigit)

type ColourToUpperBound = [(String, Int)]

colourToUpperBound :: ColourToUpperBound = [("red", 12), ("green", 13), ("blue", 14)]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let gameIds = map parseLine $ lines input
  print $ sum gameIds

parseLine :: String -> Int
parseLine string =
  let (game_header, _ : gameData) = span (/= ':') string
   in if isValidGame gameData then (read $ drop 5 game_header) :: Int else 0

getColourUpperBound :: String -> [(String, Int)] -> Int
getColourUpperBound colourString colourToUpperBound =
  let [(_, upperBound)] = filter (\(c, _) -> c == colourString) colourToUpperBound
   in upperBound

delimiter c = c /= ';' && c /= ','

isValidGame :: String -> Bool
isValidGame "" = True
isValidGame gameData =
  let (countAndColour, rest) = span delimiter gameData
      (count, _ : colour) = span (/= ' ') $ dropWhile (== ' ') countAndColour
      countInt :: Int = read count
      upperBound = getColourUpperBound colour colourToUpperBound
   in countInt <= upperBound && isValidGame (dropWhile (not . delimiter) rest)

   -- NOTE: The solution is 2551
