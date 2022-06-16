module Y15.D21 where

import           Imports
import           Parser

data ItemClass = Weapon | Armor | Ring
    deriving (Show, Eq)

data Item = Item
    { itemClass :: ItemClass
    , itemName  :: String
    , itemStat  :: Stat
    } deriving Show

data Stat = Stat
    { statCost   :: Int
    , statDamage :: Int
    , statArmor  :: Int
    } deriving Show

data Character = Character
    { characterHp     :: Int
    , characterDamage :: Int
    , characterArmor  :: Int
    } deriving Show

shopItems :: [Item]
shopItems =
    --     class   name                 $ DM AR
    [ Item Weapon "Dagger"     $ Stat   8  4  0
    , Item Weapon "Shortsword" $ Stat  10  5  0
    , Item Weapon "Warhammer"  $ Stat  25  6  0
    , Item Weapon "Longsword"  $ Stat  40  7  0
    , Item Weapon "Greataxe"   $ Stat  74  8  0

    , Item Armor  "Leather"    $ Stat  13  0  1
    , Item Armor  "Chainmail"  $ Stat  31  0  2
    , Item Armor  "Splintmail" $ Stat  53  0  3
    , Item Armor  "Bandedmail" $ Stat  75  0  4
    , Item Armor  "Platemail"  $ Stat 102  0  5

    , Item Ring   "Damage +1"  $ Stat  25  1  0
    , Item Ring   "Damage +2"  $ Stat  50  2  0
    , Item Ring   "Damage +3"  $ Stat 100  3  0
    , Item Ring   "Defense +1" $ Stat  20  0  1
    , Item Ring   "Defense +2" $ Stat  40  0  2
    , Item Ring   "Defense +3" $ Stat  80  0  3
    ]

-- Hit Points: 104
-- Damage: 8
-- Armor: 1
character :: Parser Character
character = Character
    <$> (string "Hit Points:" *> pad *> natural <* eol)
    <*> (string "Damage:"     *> pad *> natural <* eol)
    <*> (string "Armor:"      *> pad *> natural <* eol)

combs :: Int -> [a] -> [[a]]
combs 0 _      = [[]]
combs _ []     = []
combs m (x:xs) = map (x:) (combs (m-1) xs) ++ combs m xs

playerDefeatsBoss :: Character -> Character -> Bool
playerDefeatsBoss
    (Character playerHp playerDamage playerArmor)
    (Character bossHp   bossDamage bossArmor) =
    hit True playerHp (playerArmor - bossDamage) bossHp (bossArmor - playerDamage)
  where
    hit aTurn aHp aHpDelta bHp bHpDelta =
        -- NOTE not checking if both can't scratch each other
        let bHpNew = bHp + bHpDelta
        in if bHpNew <= 0
            then aTurn
            else hit (not aTurn) bHpNew bHpDelta aHp aHpDelta

playerBaseHp :: Int
playerBaseHp = 100

loadouts :: [Item] -> [[Item]]
loadouts items =
    [ weapon ++ armor ++ rings
    | weapon <- [1]    >>= flip combs (keep Weapon) --   1 weapon
    , armor  <- [0..1] >>= flip combs (keep Armor)  -- 0-1 armor
    , rings  <- [0..2] >>= flip combs (keep Ring)   -- 0-2 rings
    ]
  where
    keep c = filter (\Item{itemClass} -> itemClass == c) items

loadout2stat :: [Item] -> Stat
loadout2stat =
      foldl'
        (\(Stat a b c) (Stat x y z) -> Stat (a+x) (b+y) (c+z))
        (Stat 0 0 0)
    . fmap itemStat

solve1 :: String -> Int
solve1 s =
    let boss = parseOrDie character s
    in loadouts shopItems
        & fmap loadout2stat
        & filter (\Stat{statDamage,statArmor} -> playerDefeatsBoss (Character playerBaseHp statDamage statArmor) boss)
        & fmap statCost
        & minimum

solve2 :: String -> Int
solve2 s =
    let boss = parseOrDie character s
    in loadouts shopItems
        & fmap loadout2stat
        & filter (\Stat{statDamage,statArmor} -> not $ playerDefeatsBoss (Character playerBaseHp statDamage statArmor) boss)
        & fmap statCost
        & maximum
