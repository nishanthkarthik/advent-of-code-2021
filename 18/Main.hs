import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust)
import Data.List (elemIndex)

import Text.Megaparsec (parseMaybe, parseTest, Parsec, try, choice)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import Control.Monad (void)
import Control.Applicative
import Data.Text (Text, pack)

type Parser = Parsec Void Text

data Expr = Leaf Int | Node Expr Expr deriving (Show, Eq)

parseExpr :: Parser Expr
parseExpr = do
    void $ char '['
    l <- try $ choice [Leaf <$> decimal, parseExpr]
    void $ char ','
    r <- try $ choice [Leaf <$> decimal, parseExpr]
    void $ char ']'
    return $ Node l r

readInput :: IO [Expr]
readInput = do
    text <- readFile "18/input.txt"
    return $ map (fromJust . parseMaybe parseExpr . pack) (lines text)

data Direction = L | R deriving (Show, Eq)

findExplode :: Expr -> Int -> [Direction] -> [[Direction]]
findExplode expr i path
    | i > 4 = []
    | otherwise = case expr of
                    (Node (Leaf _) (Leaf _)) -> [reverse path | i == 4]
                    (Node l r) -> findExplode l (i + 1) (L : path) ++ findExplode r (i + 1) (R : path)
                    (Leaf _) -> []

inOrder :: [Direction] -> Expr -> [[Direction]]
inOrder dir (Node a b) = inOrder (L : dir) a ++ inOrder (R : dir) b
inOrder dir (Leaf i) = [reverse dir]

data InOrderDirection = Pre | Post deriving (Eq, Show)

inOrderNeighbor :: InOrderDirection -> [Direction] -> Expr -> Maybe [Direction]
inOrderNeighbor dir node expr = if dir == Pre then fmap (items !!) pre else fmap (items !!) suc
    where items = inOrder [] expr
          idx = (fromJust . elemIndex node) items
          pre = if idx > 0 then Just (idx - 1) else Nothing
          suc = if idx < (length items - 1) then Just (idx + 1) else Nothing

getLeaf :: Expr -> [Direction] -> Int
getLeaf (Leaf a) [] = a
getLeaf (Node l r) (d:ds) = if d == L then getLeaf l ds else getLeaf r ds

setNode :: Expr -> [Direction] -> Expr -> Expr
setNode _ [] a = a
setNode (Node l r) (d:ds) a = case d of
                                   L -> Node (setNode l ds a) r
                                   R -> Node l (setNode r ds a)

explode :: Expr -> Expr
explode expr = if null explodes then expr else afterSelf
    where explodes = findExplode expr 0 []
          edir = head explodes
          pre = inOrderNeighbor Pre (edir ++ [L]) expr
          suc = inOrderNeighbor Post (edir ++ [R]) expr
          newPre = fmap ((+ getLeaf expr (edir ++ [L])) . getLeaf expr) pre
          newSuc = fmap ((+ getLeaf expr (edir ++ [R])) . getLeaf expr) suc
          afterPre = if isJust newPre then setNode expr (fromJust pre) (Leaf $ fromJust newPre) else expr
          afterSuc = if isJust newSuc then setNode afterPre (fromJust suc) (Leaf $ fromJust newSuc) else afterPre
          afterSelf = setNode afterSuc edir (Leaf 0)

findSplit :: Expr -> [Direction] -> [(Int, [Direction])]
findSplit (Leaf a) dir = [(a, reverse dir) | a >= 10]
findSplit (Node l r) dir = findSplit l (L : dir) ++ findSplit r (R : dir)

split :: Expr -> Expr
split expr = if null splits then expr else splitExpr
    where splits = findSplit expr []
          (val, spdir) = head splits
          splitNode = Node (Leaf (div val 2)) (Leaf (div (val + 1) 2))
          splitExpr = setNode expr spdir splitNode

showExpr :: Expr -> String
showExpr (Leaf a) = show a
showExpr (Node l r) = "[" ++ showExpr l ++ "," ++ showExpr r ++ "]"

takeSteady :: (Eq a) => [a] -> a
takeSteady (a : b : rest) = if a == b then a else takeSteady (b : rest)

reduceExpr :: Expr -> Expr
reduceExpr expr = takeSteady (iterate reduceExprStep expr)
    where reduceExprStep expr = split (takeSteady (iterate explode expr))

magnitude :: Expr -> Int
magnitude (Node l r) = 3 * magnitude l + 2 * magnitude r
magnitude (Leaf a) = a

main :: IO ()
main = do
    i <- readInput

    let reduce2 a b = reduceExpr $ Node a b
        solve1 = foldl1 reduce2 i
    putStrLn $ showExpr solve1
    print $ magnitude solve1

    let combinations = [(x, y) | x <- i, y <- i, x /= y]
        solved = map (uncurry reduce2) combinations
    print $ maximum $ map magnitude solved
