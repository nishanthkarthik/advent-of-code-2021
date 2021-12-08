import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Control.Monad (void)
import Data.List (sort, scanl1, scanr1, delete, (\\), group, lookup, elemIndex)

type Parser = Parsec Void Text

type Digit = String
type Digits = [Digit]
type Line = (Digits, Digits)

parserDigits :: Parser Digits
parserDigits = some $ do
    digits <- some lowerChar
    void hspace
    return digits

parser :: Parser [Line]
parser = many $ do
    l <- parserDigits

    void hspace
    void $ char '|'
    void hspace

    r <- parserDigits
    void space
    return (l, r)

readInput :: String -> IO [Line]
readInput f = do
    text <- readFile f
    return (fromRight [] $ parse parser "" $ pack text)

withLength :: Int -> [Digit] -> Digit
withLength n xs = head $ filter ((== n) . length) xs

hasCardinality :: Int -> String -> String
hasCardinality n xs = [(head . head . filter (\i -> n == length i)) $ group $ sort xs]

solve :: [Digit] -> [(String, String)]
solve xs = zip [a,b,c,d,e,f,g] (map (: []) ['a'..'g'])
    where
        union = concat xs
        a = withLength 3 xs \\ withLength 2 xs
        b = hasCardinality 6 union
        e = hasCardinality 4 union
        f = hasCardinality 9 union
        c = withLength 2 xs \\ f
        d = withLength 4 xs \\ (b ++ c ++ f)
        g = hasCardinality 7 (union \\ (concat $ replicate 7 d))

testArr = words "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"

mapDefault :: [(String, String)] -> String -> String
mapDefault table input = concat $ map (\it -> fromJust $ lookup (it : []) table) input

solveDigit :: ([Digit], [Digit]) -> Int
solveDigit (ref, input) = digitsToNum $ map (fromJust . (flip elemIndex refArr) . sort . (mapDefault table)) input
    where
        refArr = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]
        table = solve ref
        digitsToNum = foldl (\acc it -> acc * 10 + it) 0

main :: IO ()
main = do
    i <- readInput "8/input.txt"
    print $ sum $ map (length . filter ((flip elem [2,4,3,7]) . length) . snd) i
    print $ sum $ map solveDigit i
