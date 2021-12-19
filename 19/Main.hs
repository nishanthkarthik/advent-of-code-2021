import Text.Megaparsec (parseMaybe, parseTest, Parsec, some, optional)
import Text.Megaparsec.Char (char, string, space, newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Void (Void)
import Control.Monad (void)
import Data.Text (Text, pack)

import Data.Maybe (fromJust, isJust)
import Data.List (sortOn, sort, group, transpose, partition)
import qualified Data.Set as Set

type Parser = Parsec Void Text
type Coords = Set.Set (Int, Int, Int)

parser :: Parser [Coords]
parser = some $ do
    optional $ void newline
    void $ string $ pack "--- scanner "
    void $ decimal
    void $ string $ pack " ---"
    void newline

    let parseSigned = signed space decimal
    coords <- some $ do
        a <- parseSigned
        void $ char ','
        b <- parseSigned
        void $ char ','
        c <- parseSigned
        optional $ void newline
        return (a, b, c)
    return $ Set.fromList coords

readInput :: IO [Coords]
readInput = do
    text <- pack <$> readFile "19/input.txt"
    return $ (fromJust . parseMaybe parser) text

rot :: (Int, Int, Int) -> [(Int, Int, Int)]
rot (x, y, z) = concatMap rot [(x, y, z), (x, z, -y), (-x, -y, z), (-x, -z, -y), (-x, y, -z), (-x, z, y), (x, -y, -z), (x, -z, y)]
    where rot (a, b, c) = [(a, b, c), (b, c, a), (c, a, b)]

maxFreq :: (Eq a, Ord a) => [a] -> (a, Int)
maxFreq = last . sortOn snd . map (\it -> (head it, length it)) . group . sort

type Offset = (Int, Int, Int)

match :: Coords -> Coords -> Maybe (Int, (Coords, Offset))
match refs coords = if freq >= 12 then Just (freq, (newCoords, offset)) else Nothing
    where rots = (map Set.fromList . transpose . map rot . Set.toList) coords
          poss = [(rs, maxFreq [subTuple l r | l <- Set.toList rs, r <- Set.toList refs]) | rs <- rots]
          (rotn, (offset, freq)) = (last . sortOn (snd . snd)) poss
          newCoords = Set.map (`subTuple` offset) rotn
          subTuple (a, b, c) (d, e, f) = (a - d, b - e, c - f)

solver :: Coords -> [Coords] -> [Offset] -> (Coords, [Offset])
solver origin [] offsets = (origin, offsets)
solver origin candidates offsets = solver (Set.union origin selected) (init sortedSelecteds ++ rest) (offset : offsets)
    where (selecteds, rest) = partition (isJust . match origin) candidates
          sortedSelecteds = sortOn (fst . fromJust . match origin) selecteds
          (freq, (selected, offset)) = fromJust $ match origin $ last sortedSelecteds

main :: IO ()
main = do
    i <- readInput
    let origin = head i
        (points, offsets) = solver origin (tail i) [(0,0,0)]
        solve1 = Set.size points
    print solve1

    let manhattan (a, b, c) (x, y, z) = abs (a - x) + abs (b - y) + abs (c - z)
        solve2 = maximum [manhattan l r | l <- offsets, r <- offsets, l /= r]
    print solve2
