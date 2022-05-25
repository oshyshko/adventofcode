module Y21.D16 where

import           Imports
import           Util

data Packet = Packet
    { version :: Int
    , op      :: Op
    , args    :: [Packet]
    } deriving (Show, Eq)

data Op
    = Sum
    | Product
    | Minimum
    | Maximum
    | Literal Int
    | Greater
    | Lesser
    | Equal
    deriving (Show, Eq)

packetPadded :: Parser Packet
packetPadded =
    packet <* many (char '0')
  where
    packet :: Parser Packet
    packet = do
        ver <- bin2int <$> count 3 bit
        tid <- bin2int <$> count 3 bit
        uncurry (Packet ver {- op args -}) <$>
            if tid == 4
            then -- (Literal x, [])
                (,[]) . Literal . bin2int <$> flip fix [] \loop xs -> do
                    c:bits4 <- count 5 bit
                    (if c == '1' then loop else pure) (xs ++ bits4)
            else -- (<all other Ops>, [args])
                (tid2op tid,) <$> (bit >>= \case
                    '0' -> do
                        totalLength <- bin2int <$> count 15 bit
                        s <- count totalLength bit
                        pure $ parseOrDie (many packet) s   -- TODO refactor
                    _   -> do
                        subpacketCount <- bin2int <$> count 11 bit
                        count subpacketCount packet)
    tid2op :: Int -> Op
    tid2op = \case
        0 -> Sum
        1 -> Product
        2 -> Minimum
        3 -> Maximum
        4 -> error "Unexpected type id: 4"
        5 -> Greater
        6 -> Lesser
        7 -> Equal
        n -> error $ "Unknown type id: " <> show n
    bit :: Parser Char
    bit = char '0' <|> char '1'

-- > hex2bin "12312453AFFDAB12"
-- "0001001000110001001001000101001110101111111111011010101100010010"
hex2bin :: String -> String
hex2bin = ((printf "%04b" . digitToInt) =<<)

-- > bin2int "010011"
-- 19
bin2int :: String -> Int
bin2int = foldl' (\a x -> a * 2 + digitToInt x) 0

solve1 :: String -> Int
solve1 =
    sumOfVersions . parseOrDie packetPadded . hex2bin . head . lines
  where
    sumOfVersions :: Packet -> Int
    sumOfVersions (Packet ver _ cs) = ver + sum (sumOfVersions <$> cs)

solve2 :: String -> Int
solve2 =
    eval . parseOrDie packetPadded . hex2bin . head . lines
  where
    eval :: Packet -> Int
    eval (Packet _ op cs) =
        (eval <$> cs) & case op of
            Sum       -> sum
            Product   -> product
            Literal x -> \[] -> x
            Minimum   -> minimum
            Maximum   -> maximum
            Greater   -> \[a,b] -> if a >  b then 1 else 0
            Lesser    -> \[a,b] -> if a <  b then 1 else 0
            Equal     -> \[a,b] -> if a == b then 1 else 0
