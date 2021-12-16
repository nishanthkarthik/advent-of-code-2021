import Data.Char (digitToInt, isHexDigit, intToDigit)
import Data.Bits (testBit)
import Data.Maybe (fromJust)

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)
import Data.Text (Text, pack)

intToBytes :: Int -> [Char]
intToBytes x = map (intToDigit . fromEnum . testBit x) [3,2..0]

decodeHex :: String -> Text
decodeHex i = (pack . concatMap (intToBytes . digitToInt) . filter isHexDigit) i

readInput :: String -> IO Packet
readInput f = do
    text <- readFile f
    let parsed = parseMaybe parser $ decodeHex text
    return $ fromJust parsed

binStringToInt :: String -> Int
binStringToInt = (foldl (\ac it -> ac * 2 + it) 0) . map digitToInt

type Parser = Parsec Void Text

parseChunks :: Parser Int
parseChunks = do
    chunks <- many . try $ do
        void $ char '1'
        chunkData <- count 4 binDigitChar
        return chunkData

    void $ char '0'
    lastChunk <- count 4 binDigitChar
    return $ binStringToInt (concat chunks ++ lastChunk)

nDigitNum :: Int -> Parser Int
nDigitNum n = binStringToInt <$> count n binDigitChar

data Packet = LiteralPacket Int Int Int
            | OperatorPacket Int Int [Packet] deriving (Show, Eq)

parsePacket :: Parser Packet
parsePacket = do
    version <- nDigitNum 3
    pType <- nDigitNum 3

    lengthId <- if (let isOperatorPacket = (/= 4) in isOperatorPacket pType)
                then Just <$> binDigitChar
                else return Nothing

    result <- case lengthId of
                        Nothing -> do
                            value <- parseChunks
                            return (LiteralPacket version pType value)
                        Just '0' -> do
                            totalLengthSub <- nDigitNum 15
                            subchunk <- pack <$> count totalLengthSub binDigitChar
                            let nested = (fromJust . parseMaybe (some parsePacket)) subchunk
                            return (OperatorPacket version pType nested)
                        Just '1' -> do
                            numSub <- nDigitNum 11
                            nested <- count numSub parsePacket
                            return (OperatorPacket version pType nested)
    return result

parser :: Parser Packet
parser = do
    packet <- parsePacket
    void $ many $ char '0'
    return packet

versionSum :: Packet -> Int
versionSum (LiteralPacket i _ _) = i
versionSum (OperatorPacket i _ c) = i + (sum . map versionSum) c

calc :: Packet -> Int
calc (LiteralPacket _ _ v) = v
calc (OperatorPacket _ 0 cs) = foldl (+) 0 $ map calc cs
calc (OperatorPacket _ 1 cs) = foldl (*) 1 $ map calc cs
calc (OperatorPacket _ 2 cs) = foldl1 min $ map calc cs
calc (OperatorPacket _ 3 cs) = foldl1 max $ map calc cs
calc (OperatorPacket _ 5 (cs1 : cs2 : [])) = fromEnum (calc cs1 > calc cs2)
calc (OperatorPacket _ 6 (cs1 : cs2 : [])) = fromEnum (calc cs1 < calc cs2)
calc (OperatorPacket _ 7 (cs1 : cs2 : [])) = fromEnum (calc cs1 == calc cs2)

main :: IO ()
main = do
    i <- readInput "16/input.txt"
    print $ versionSum i
    print $ calc i
