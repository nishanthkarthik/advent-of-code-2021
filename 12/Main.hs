import Text.Megaparsec
import Text.Megaparsec.Char (letterChar, char, newline)
import Data.Void (Void)
import Data.Text (Text, pack, all)
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

parser :: Parser [(Text, Text)]
parser = many $ do
    from <- some letterChar
    void $ char '-'
    to <- some letterChar
    void $ optional newline
    return (pack from, pack to)

type Visitor = [Text] -> [Text] -> [Text]
type Graph = Map.Map Text [Text]

visitor1 :: Visitor
visitor1 visited neighbors = manyTimes ++ (oneTime \\ visited)
    where (manyTimes, oneTime) = partition (Data.Text.all isUpper) neighbors

visitor2 :: Visitor
visitor2 visited neighbors = upper ++ lowerVisits
    where (upper, lower) = partition (Data.Text.all isUpper) (neighbors \\ [pack "start"])
          countElem arr x = (length . filter (== x)) arr
          visitedLower = filter (Data.Text.all isLower) visited
          alreadyVisitedALowerTwice = (length visitedLower) /= (length $ nubOrd visitedLower)
          lowerVisits = if alreadyVisitedALowerTwice then (lower \\ visited) else lower

-- Adj list -> Visitor -> Visited -> Cur -> PathCount
bfs :: Graph -> Visitor -> [Text] -> Text -> Int
bfs graph visitor visited cur
    | cur == (pack "end") = 1
    | otherwise = sum $ map (bfs graph visitor (cur : visited)) visits
    where visits = visitor (cur : visited) (graph Map.! cur)

readInput :: String -> IO [(Text, Text)]
readInput f = do
    text <- readFile f
    return (fromRight [] $ parse parser "" $ pack text)

main :: IO ()
main = do
    i <- readInput "12/input.txt"
    let graph = makeGraph i
        makeGraph :: [(Text, Text)] -> Graph
        makeGraph xs = Map.fromList keyvals
        undirected xs = filter (\(a, b) -> a /= (pack "end") && b /= (pack "start")) (xs ++ map swap xs)
        pairs = groupBy ((==) `on` fst) $ sort $ undirected i
        keyvals = map (\it -> ((fst . head) it, map snd it)) pairs

    print $ bfs graph visitor1 [] $ pack "start"
    print $ bfs graph visitor2 [] $ pack "start"
