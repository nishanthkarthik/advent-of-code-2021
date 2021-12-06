{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad (void)
import Data.List (sort, group, concatMap)
import Data.Either

type Parser = Parsec Void Text

type Segment = (Int, Int, Int, Int)
type Point = (Int, Int)

segmentsParser :: Parser [Segment]
segmentsParser = many $ do
    a <- Lex.decimal
    void (char ',')
    b <- Lex.decimal
    void (string " -> ")
    c <- Lex.decimal
    void (char ',')
    d <- Lex.decimal
    void (optional newline)
    return (a, b, c, d)

readInput :: String -> IO [Segment]
readInput f = do
    text <- readFile f
    return (fromRight [] $ parse segmentsParser "" $ pack text)

horizontalOrVertical :: Segment -> Bool
horizontalOrVertical (a,b,c,d) = a == c || b == d

points :: Segment -> [(Int, Int)]
points (a, b, c, d)
    | a == c = zip (repeat a) $ range b d
    | b == d = zip (range a c) $ repeat b
    | abs (c - a) == abs (d - b) = zip (range a c) (range b d)
    | otherwise = []
    where range x y = if (x < y) then [x..y] else [x,x-1..y]

main :: IO ()
main = do
    input <- readInput "5/input.txt"
    let p1Input = filter horizontalOrVertical input
        p2Input = input
        overlapping = length . filter ((>= 2) . length) . group . sort . concatMap points
    print $ overlapping p1Input
    print $ overlapping p2Input
