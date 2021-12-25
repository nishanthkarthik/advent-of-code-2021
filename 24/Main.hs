import Text.Megaparsec (Parsec, some, many, try, choice, optional, parseMaybe, parseTest)
import Text.Megaparsec.Char (space, string, lowerChar, hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Void (Void)
import Control.Monad (void)
import Data.Text (pack, Text)
import Data.Maybe (fromJust, mapMaybe)
import Control.Parallel.Strategies (parMap, rpar)

type Parser = Parsec Void Text

data Val = Reg Char | Lit Int deriving (Show, Eq)
data Op = Inp Val | Add Val Val | Mul Val Val | Div Val Val | Mod Val Val | Eql Val Val deriving (Show, Eq)

parseVal :: Parser Val
parseVal = optional hspace *> choice [Reg <$> lowerChar, Lit <$> signed space decimal]

parseOp :: Parser Op
parseOp = choice
    [
        Inp <$> (pkStr "inp" *> parseVal),
        binaryOp Add "add",
        binaryOp Mul "mul",
        binaryOp Div "div",
        binaryOp Mod "mod",
        binaryOp Eql "eql"
    ] <* many newline
    where pkStr = string . pack
          twoVals = (,) <$> parseVal <*> parseVal
          binaryOp f tok = uncurry f <$> (pkStr tok *> twoVals)

readInput :: IO [Op]
readInput = fromJust . parseMaybe (some parseOp) . pack <$> readFile "24/input.txt"

chunkBy :: Int -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy n xs = take n xs : chunkBy n (drop n xs)

type Variants = (Int, Int, Int)

constants :: [Op] -> [Variants]
constants xs = map fn chunks
    where chunkLength = length xs `div` 14
          chunks = chunkBy chunkLength xs
          fn chunk = (divArg (chunk !! 4), addArg (chunk !! 5), addArg (chunk !! 15))
          addArg (Add _ (Lit lit)) = lit
          divArg (Div _ (Lit lit)) = lit

zFunc :: Int -> Variants -> Int -> Int
zFunc z (k5, k6, k16) i = if br then lb else rb
    where br = i /= (mod z 26 + k6)
          lb = div z k5 * 26 + i + k16;
          rb = div z k5;

dfs :: [Int] -> [Int] -> [Variants] -> Int -> [Int]
dfs gen states vars z
    | z == 0 && null vars = states
    | null vars = []
    | otherwise = if null corr then [] else head corr
    where soln = map (\i -> dfs gen (i : states) (tail vars) (zFunc z var i)) is
          corr = dropWhile null soln
          var = head vars
          is = if k5 var == 1 then gen else filter (== (mod z 26 + k6 var)) gen
          k5 (a, _, _) = a
          k6 (_, b, _) = b

main :: IO ()
main = do
    i <- constants <$> readInput
    print $ (concatMap show . reverse) (dfs [9,8..1] [] i 0)
    print $ (concatMap show . reverse) (dfs [1..9] [] i 0)
