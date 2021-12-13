import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import Control.Monad (void, mapM_)
import Data.Text (Text, pack, chunksOf)
import Data.Either (fromRight)
import Data.Containers.ListUtils (nubOrd)
import Data.Set (member, fromList)

type Parser = Parsec Void Text

type Dot = (Int, Int)
type Fold = (Char, Int)
data Input = Input { dots :: [Dot], folds :: [Fold] } deriving (Show, Eq)

parser :: Parser Input
parser = do
    dots <- some $ do
        x <- decimal
        void (char ',')
        y <- decimal
        void newline
        return (x, y)
    void newline
    folds <- some $ do
        void (string $ pack "fold along")
        void space
        axis <- lowerChar
        void (char '=')
        position <- decimal
        void newline
        return (axis, position)
    return $ Input dots folds

readInput :: String -> IO Input
readInput f = do
    text <- readFile f
    let defaultValue = Input [] []
    return (fromRight defaultValue $ parse parser "" $ pack text)

foldY :: Int -> Dot -> Dot
foldY y0 (x, y)
    | y <= y0 = (x, y)
    | otherwise = (x, y0 - (y - y0))

foldX :: Int -> Dot -> Dot
foldX x0 (x, y)
    | x <= x0 = (x, y)
    | otherwise = (x0 - (x - x0), y)

foldT :: Fold -> Dot -> Dot
foldT (axis, pos)
    | axis == 'x' = foldX pos
    | axis == 'y' = foldY pos

visualizeGrid :: [Dot] -> [Text]
visualizeGrid dots = chunksOf width $ pack base
    where
        maxX = foldl1 max $ map fst dots
        maxY = foldl1 max $ map snd dots
        width = maxY + 1
        set = fromList dots
        generator it = if (member it set) then '#' else '.'
        base = map generator [(x, y) | x <- [0..maxX], y <- [0..maxY]]

main :: IO ()
main = do
    (Input dots folds) <- readInput "13/input.txt"
    print $ length $ nubOrd $ map (foldT $ head folds) dots
    let finalPaper = foldl (\ac it -> map (foldT it) ac) dots folds
    mapM_ print $ visualizeGrid finalPaper
