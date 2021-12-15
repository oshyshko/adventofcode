module Y21.D03 where

import           Imports

type Bit = Bool

-- 00100
-- 11110
-- 10110
parseInput :: String -> [[Bit]]
parseInput =
    map (map char2bool) . lines
  where
    char2bool = \case
        '0' -> False
        '1' -> True
        e   -> error $ "unexpected: " <> show e

bits2int :: [Bit] -> Int
bits2int =
    fst . foldl' f (0,1) . reverse
  where
    f (a, weight) x = (a + if x then weight else 0, weight * 2)

dominating :: (Int -> Int -> Bool) -> [Bit] -> Bit
dominating cmp xs =
    let (zs,os) = partition (== False) xs
    in cmp (length zs) (length os)

solve1 :: String -> Int
solve1 =
      (\gamma ->
        let epsilon = map not gamma
        in bits2int gamma * bits2int epsilon)
    . map (dominating (>=))
    . transpose
    . parseInput

solve2 :: String -> Int
solve2 =
      (\xs ->
          let oxygen = scrub (>)  xs
              co2    = scrub (<=) xs
          in bits2int oxygen * bits2int co2)
    . parseInput
  where
    scrub :: (Int -> Int -> Bit) -> [[Bit]] -> [Bit]
    scrub _   [x]   = x
    scrub _   ([]:_) = error "couldn't find a unique match"
    scrub cmp xs =
        let d = dominating cmp $ map head xs
        in (d:) . scrub cmp . map tail . filter ((== d) . head) $ xs
