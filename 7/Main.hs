import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Either (fromRight)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad (void)

type Parser = Parsec Void Text

parser :: Parser [Integer]
parser = many $ do
    void $ optional (char ',')
    a <- Lex.decimal
    return a

readInput :: String -> IO [Integer]
readInput f = do
    text <- readFile f
    return (fromRight [] $ parse parser "" $ pack text)

main :: IO ()
main = do
    i <- readInput "7/input.txt"
    let ap n = (n * (n + 1)) `div` 2
        stoplimit = 2 * (foldl1 max i)
    print $ foldl1 min [sum $ map (abs . subtract it) i | it <- i]
    print $ foldl1 min [sum $ map (ap . abs . subtract it) i | it <- [0..stoplimit]]
