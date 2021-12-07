import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Either (fromRight)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad (void)
import Data.List (sort, scanl1, scanr1)

type Parser = Parsec Void Text

parser :: Parser [Int]
parser = many $ do
    void $ optional (char ',')
    a <- Lex.decimal
    return a

readInput :: String -> IO [Int]
readInput f = do
    text <- readFile f
    return (fromRight [] $ parse parser "" $ pack text)

main :: IO ()
main = do
    i <- readInput "7/input.txt"
    let d = sort i
        presum = scanl1 (+) d
        postsum = scanr1 (+) d ++ [0]
        expr1 = (\(idx,it) -> (2 * idx + 2 - length d) * it + (postsum !! (idx + 1)) - (presum !! idx))
    print $ foldl1 min $ map expr1 $ zip [0..] d

    let total = fromIntegral (sum d) :: Double
        n = fromIntegral (length d) :: Double
        minimaj = (floor $ 0.5 + (total / n)) - 1
        ap n = (n * (n + 1)) `div` 2
    print $ foldl1 min [sum $ map (ap . abs . (subtract it)) d | it <- [minimaj-2..minimaj+2] ]
