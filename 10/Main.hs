import Data.List
import Data.Maybe

brackets = [('{', '}'), ('(', ')'), ('[', ']'), ('<', '>')]

invalidClosing :: String -> (Maybe Char, [Char])
invalidClosing xs = foldl folder (Nothing, []) xs
    where folder :: (Maybe Char, [Char]) -> Char -> (Maybe Char, [Char])
          folder (Just x, st) it = (Just x, st)
          folder (Nothing, st) it
              | elem it $ map fst brackets = (Nothing, (it : st))
              | Just it == lookup (head st) brackets = (Nothing, tail st)
              | otherwise = (Just it, st)

readInput :: String -> IO [String]
readInput f = do
    text <- readFile f
    return $ lines text

invalidCharToScore :: Maybe Char -> Int
invalidCharToScore it = case it of
                            Just ')' -> 3
                            Just ']' -> 57
                            Just '}' -> 1197
                            Just '>' -> 25137
                            Nothing -> 0

score :: Char -> Int
score it = case it of
                ')' -> 1
                ']' -> 2
                '}' -> 3
                '>' -> 4

main :: IO ()
main = do
    i <- readInput "10/input.txt"
    print $ sum $ map (invalidCharToScore . fst . invalidClosing) i

    let incompleteLines = filter (\(result, stack) -> (isNothing result && not (null stack))) (map invalidClosing i)
        fixes = map ((map (fromJust . (flip lookup brackets))) . snd) incompleteLines
        median xs = xs !! (length xs `div` 2)

    print $ median $ sort $ map ((foldl (\acc it -> acc * 5 + it) 0) . (map score)) fixes
