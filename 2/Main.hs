import Data.Complex

data Direction = Forward | Down | Up deriving (Eq, Show)

readDirection :: String -> Direction
readDirection "forward" = Forward
readDirection "down" = Down
readDirection "up" = Up

data Step = Step Direction Integer deriving (Eq, Show)

readInput :: IO [Step]
readInput = do
    text <- readFile "2/input.txt"
    return $ map (\(a:b:[]) -> Step (readDirection a) (read b :: Integer)) $ map words $ lines text

stepToVec :: Step -> Complex Double
stepToVec (Step Forward x) = fromInteger x :+ 0.0
stepToVec (Step Up x) = 0.0 :+ fromInteger (-x)
stepToVec (Step Down x) = 0.0 :+ fromInteger (x)

finalPosition :: [Step] -> Complex Double
finalPosition = foldl1 (+) . map stepToVec

finalPositionwithAim :: [Step] -> Complex Double
finalPositionwithAim = snd . foldl withAim' (0.0, 0.0 :+ 0.0) . map stepToVec
    where
        withAim' :: (Double, Complex Double) -> Complex Double -> (Double, Complex Double)
        withAim' (aim, pos) (real :+ imag) = (aim + imag, pos + (real :+ aim * real))

main = do
    i <- readInput
    let heading = finalPosition i
    print $ round $ (realPart heading) * (imagPart heading)
    let headingWithAim = finalPositionwithAim i
    print $ round $ (realPart headingWithAim) *(imagPart headingWithAim)
    return ()
