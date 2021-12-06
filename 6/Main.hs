
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Data.Either (fromRight)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad (void)

type Parser = Parsec Void Text

parser :: Parser [Integer]
parser = many $ do
    a <- Lex.decimal
    void $ optional (char ',')
    return a

readInput :: String -> IO [Integer]
readInput f = do
    text <- readFile f
    return (fromRight [] $ parse parser "" $ pack text)

-- [8¸ 7, 6, 5, 4, 3, 2, 1, 0]
updateState :: [Integer] -> [Integer]
updateState x = zipWith (+) (0 : x) (last x : 0 : last x : replicate 6 0)

main :: IO ()
main = do
    input <- readInput "6/input.txt"
    let i = map (\it -> (toInteger . length) $ filter (== it) input) [8,7..0]
    print $ sum $ iterate updateState i !! 80
    print $ sum $ iterate updateState i !! 256
