import Text.Megaparsec
import Text.Megaparsec.Char (letterChar, char, newline)
import Data.Void (Void)
import Data.Text (Text, pack)
import Control.Monad (void)
import Data.Either (fromRight)
import Data.Maybe
import Data.List
import Data.Char (isUpper, isLower)
import Data.Tuple (swap)
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import qualified Data.Map.Strict as Map

type Parser = Parsec Void Text

parser :: Parser [(String, String)]
parser = many $ do
    from <- some letterChar
    void $ char '-'
    to <- some letterChar
    void $ optional newline
    return (from, to)

type Visitor = [String] -> [String] -> [String]
type Graph = Map.Map String [String]

visitor1 :: Visitor
visitor1 visited neighbors = manyTimes ++ (oneTime \\ visited)
    where (manyTimes, oneTime) = partition (all isUpper) neighbors

visitor2 :: Visitor
visitor2 visited neighbors = upper ++ lowerVisits
    where (upper, lower) = partition (all isUpper) (neighbors \\ ["start"])
          countElem arr x = (length . filter (== x)) arr
          visitedLower = filter (all isLower) visited
          alreadyVisitedALowerTwice = (length visitedLower) /= (length $ nubOrd visitedLower)
          lowerVisits = if alreadyVisitedALowerTwice then (lower \\ visited) else lower

-- Adj list -> Visitor -> Visited -> Cur -> PathCount
bfs :: Graph -> Visitor -> [String] -> String -> Int
bfs graph visitor visited cur
    | cur == "end" = 1
    | otherwise = sum $ map (bfs graph visitor (cur : visited)) visits
    where visits = visitor (cur : visited) (graph Map.! cur)

readInput :: String -> IO [(String, String)]
readInput f = do
    text <- readFile f
    return (fromRight [] $ parse parser "" $ pack text)

main :: IO ()
main = do
    i <- readInput "12/input.txt"
    let graph = makeGraph i
        makeGraph :: [(String, String)] -> Graph
        makeGraph xs = Map.fromList keyvals
            where undirected xs = filter (\(a, b) -> a /= "end" && b /= "start") (xs ++ map swap xs)
                  pairs = groupBy ((==) `on` fst) $ sort $ undirected xs
                  keyvals = map (\it -> ((fst . head) it, map snd it)) pairs

    print $ bfs graph visitor1 [] "start"
    print $ bfs graph visitor2 [] "start"
