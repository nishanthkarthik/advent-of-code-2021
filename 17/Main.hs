import Data.Maybe (fromJust)
import Data.Containers.ListUtils (nubOrd)

import Text.Megaparsec (parseMaybe, Parsec, parseTest)
import Text.Megaparsec.Char (binDigitChar, char, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed, lexeme)
import Data.Void (Void)
import Control.Monad (void)
import Data.Text (Text, pack)

type Parser = Parsec Void Text
type Input = ((Int, Int), (Int, Int))

parser :: Parser Input
parser = do
    let parseSigned = signed space decimal
    let packedString = string . pack
    void $ packedString "target area: x="
    x1 <- parseSigned
    void $ packedString ".."
    x2 <- parseSigned
    void $ packedString ", y="
    y1 <- parseSigned
    void $ packedString ".."
    y2 <- parseSigned
    void space
    return ((x1, x2), (y1, y2))

readInput :: IO Input
readInput = do
    text <- readFile "17/input.txt"
    return $ (fromJust . parseMaybe parser) $ pack text

solve1 :: Input -> Int
solve1 input = div (v * (v + 1)) 2
    where v = abs y1 - 1
          (_, (y1, _)) = input

data RangeMode = Inc | Dec | DecInc deriving (Eq, Show)

solve2 :: RangeMode -> (Int, Int) -> Int -> [(Int, Int)]
solve2 mode (lo, up) velo = zip (repeat velo) idxs
    where velrange = case mode of
                        Inc -> [velo, velo + 1 .. up]
                        Dec -> [velo, velo - 1 .. 0] ++ replicate (10 * velo) 0
                        DecInc -> [-velo .. 0] ++ [1 .. up]
          pre = scanl1 (+) velrange
          inLimits i = lo <= i && i <= up
          idxs = (map fst . filter (inLimits . snd)) $ zip [1..] pre

main :: IO ()
main = do
    i <- readInput
    print $ solve1 i

    let ((x1, x2), (y1, y2)) = i
        u's = map (solve2 Dec (x1, x2)) [0..x2+1]
        vdown's = map (solve2 Inc (abs y2, abs y1)) [0..abs y1+1]
        overlapDown xx yy = [(fst u, -fst v) | u <- concat xx, v <- concat yy, snd u == snd v]

        vup's = map (solve2 DecInc (abs y2, abs y1)) [0..abs y1+1]
        overlapUp xx yy = [(fst u, fst v) | u <- concat xx, v <- concat yy, snd u == snd v]
        overlaps = nubOrd (overlapUp u's vup's ++ overlapDown u's vdown's)
    print $ length overlaps
