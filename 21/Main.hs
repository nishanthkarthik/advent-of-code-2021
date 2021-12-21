import Text.Megaparsec (Parsec, parseMaybe, count, optional)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import Control.Monad (void)
import Data.Text (pack, Text)
import Data.Maybe (fromJust)

import Data.Array (array, (!), range)
import Data.List (group, sort, genericLength)

type Parser = Parsec Void Text

parsePlayer :: Parser (Int, Int)
parsePlayer = do
    void $ (string . pack) "Player "
    player <- decimal
    void $ (string . pack) " starting position: "
    position <- decimal
    return (player, position)

parsePlayers :: Parser [(Int, Int)]
parsePlayers = count 2 $ do
    player <- parsePlayer
    void $ optional newline
    return player

readInput :: IO [(Int, Int)]
readInput = fromJust . parseMaybe parsePlayers . pack <$> readFile "21/input.txt"

rangeGen :: ([Int], [Int])
rangeGen = (map sum f, map sum s)
    where r = map (chunkBy 3) $ chunkBy 6 (cycle [1..100])
          f = map (!! 0) r
          s = map (!! 1) r
          chunkBy _ [] = []
          chunkBy n as = take n as : chunkBy n (drop n as)

play :: Int -> [Int] -> [(Int, Int)]
play pos throws = zip [1..] total
    where steps = tail $ scanl (\ac it -> rot (ac + it)) pos throws
          total = scanl1 (+) steps
          rot i = 1 + mod (i - 1) 10

solve1 :: Int -> Int -> Int
solve1 a b = rolls1 * loser1
    where aplay = play a (fst rangeGen)
          bplay = play b (snd rangeGen)
          game = head . dropWhile ((< 1000) . snd)
          a1000 = game aplay
          b1000 = game bplay
          awins = fst a1000 < fst b1000
          rolls1 = if awins then 3 * ((fst a1000 - 1) * 2 + 1) else 6 * fst b1000
          loser1 = if awins then snd (bplay !! (fst a1000 - 2)) else snd (aplay !! (fst b1000 - 1))

solve2 :: Int -> Int -> Integer
solve2 p1s p2s = maximum (cache ! (0, toInteger p1s, toInteger p2s, 0, 0))
    where
        -- memoize (playerTurn, position1, position2, score1, score2)
        lims = ((0, 1, 1, 0, 0), (1, 10, 10, 30, 30))
        cache = array lims [(it, thunk it) | it <- range lims]
        rot i step = 1 + mod (i + step - 1) 10
        threeDiceHisto = (map (\it -> (head it, genericLength it)) . group . sort) [i + j + k | i <- [1..3], j <- [1..3], k <- [1..3]]
        thunk :: (Integer, Integer, Integer, Integer, Integer) -> [Integer]
        thunk (turn1, p1, p2, sc1, sc2)
            | sc1 >= 21 = [1, 0]
            | sc2 >= 21 = [0, 1]
            | turn1 == 0 = sumPairs $ map (\(i, fr) -> map (* fr) (tlook 1 (rot p1 i) p2 (sc1 + rot p1 i) sc2)) threeDiceHisto
            | otherwise = sumPairs $ map (\(i, fr) -> map (* fr) (tlook 0 p1 (rot p2 i) sc1 (sc2 + rot p2 i))) threeDiceHisto
            where tlook a b c d e = cache ! (a, b, c, d, e)
                  sumPairs = foldl1 (zipWith (+))

main :: IO ()
main = do
    [(_, a), (_, b)] <- readInput
    print $ solve1 a b
    print $ solve2 a b
