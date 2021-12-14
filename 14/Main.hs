import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Either (fromRight)
import Data.List (sort, group, sortOn, genericLength, genericIndex)
import qualified Data.Map.Strict as Map

type Parser = Parsec Void Text

type Polymer = [Char]
type Rules = Map.Map (Char, Char) Char

parser :: Parser (Polymer, Rules)
parser = do
    poly <- some upperChar
    void space
    rules <- some $ do
        from <- (,) <$> upperChar <*> upperChar
        void $ string $ pack " -> "
        to <- upperChar
        void $ optional newline
        return (from, to)
    return (poly, Map.fromList rules)

readInput :: String -> IO (Polymer, Rules)
readInput f = do
    text <- readFile f
    let defaultValue = ([], Map.empty)
    return (fromRight defaultValue $ parse parser "" $ pack text)

frequencies :: (Eq a, Ord a) => [a] -> [(a, Integer)]
frequencies xs = sortOn snd $ (map (\it -> (head it, genericLength it)) . group . sort) xs

type Workset = Map.Map (Char, Char) Integer

expand :: Rules -> Workset -> Workset
expand r w = delta
    where
        subOne it@(a, b) = zip [(a, r Map.! it), (r Map.! it, b)] $ repeat (w Map.! it)
        delta = Map.fromListWith (+) $ concatMap subOne $ Map.keys w

aggregate :: Workset -> (Char, Char) -> [(Char, Integer)]
aggregate w (begin, end) = sortOn snd $ Map.toList $ Map.fromListWith (+) ((begin, 1) : (end, 1) : freq)
    where freq = concatMap (\((a, b), v) -> [(a, v), (b, v)]) $ Map.toList w

solve :: (Polymer, Rules) -> Integer -> Integer
solve (poly, rules) n = div (foldl1 max freq - foldl1 min freq) 2
    where
        pairs = (zip poly $ tail poly)
        workset = (Map.fromList . frequencies) pairs
        iter = iterate (expand rules) workset
        worksetN = genericIndex iter n
        hist = aggregate worksetN (head poly, last poly)
        freq = map snd hist

main :: IO ()
main = do
    input <- readInput "14/input.txt"
    print $ solve input 10
    print $ solve input 40
