data Vec2i = Integer :+ Integer deriving (Eq, Show)

infixl 6 ++:
(a :+ b) ++: (c :+ d) = (a + c) :+ (b + d)

readInput :: IO [Vec2i]
readInput = do
    text <- readFile "2/input.txt"
    return $ map (parseWords . words) $ lines text
        where
            parseWords (a : b : []) = direction a (read b :: Integer)
            direction x i = case x of
                "forward" -> i :+ 0
                "up" -> 0 :+ (-i)
                "down" -> 0 :+ i

finalPositionwithAim :: [Vec2i] -> Vec2i
finalPositionwithAim = snd . foldl withAim' (0, 0 :+ 0)
    where
        withAim' :: (Integer, Vec2i) -> Vec2i -> (Integer, Vec2i)
        withAim' (aim, pos) (a :+ b) = (aim + b, pos ++: (a :+ (aim * a)))

main = do
    i <- readInput
    let
        solution :: Vec2i -> Integer
        solution (a :+ b) = a * b
    print $ solution $ foldl1 (++:) i
    print $ solution $ finalPositionwithAim i
    return ()
