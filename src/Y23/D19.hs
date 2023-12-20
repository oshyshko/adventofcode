module Y23.D19 where

import           Geom.Range (Range (..), intersection)
import           Imports
import           Parser
import           Util

type WorkflowName = String
type Cat          = Char                -- x,m,a,s
type Rule         = (Cond, WorkflowName)
data Cond         = Less Cat Int | Greater Cat Int | Always deriving Show
type Workflow     = (WorkflowName, [Rule])
type Part         = [(Cat,RangeInt)]
type RangeInt     = Range Int Int

-- px{a<2006:qkq,m>2090:A,rfg}
-- pv{a>1716:R,A}
--
-- {x=787,m=2655,a=1222,s=2876}
-- {x=1679,m=44,a=2067,s=496}
workflowsAndParts :: Parser ([Workflow], [Part])
workflowsAndParts =
    (,) <$> (workflow `endBy` eol) <* eol
        <*> (part `endBy` eol)
  where
    workflowName = many1 letter
    workflow = (,)
        <$> workflowName
        <* char '{' <*> rule `sepBy` char ',' <* char '}'
    rule =
        (,) <$> (   try (Less    <$> (cat <* char '<') <*> natural <* char ':')
                <|> try (Greater <$> (cat <* char '>') <*> natural <* char ':')
                <|> pure Always)
            <*> workflowName
    part = mkPart
        <$> (char '{' *> cat <* char '=' *> range) <*> (char ',' *> cat <* char '=' *> range)
        <*> (char ',' *> cat <* char '=' *> range) <*> (char ',' *> cat <* char '=' *> range)
        <* char '}'
    cat = oneOf "xmas"
    range = (\o -> Range {offset=o, size=1}) <$> natural

mkPart :: a ~ RangeInt => a -> a -> a -> a -> Part
mkPart x m a s = [('x',x), ('m',m), ('a',a), ('s',s)]

countAcceptableCombs :: [Workflow] -> Part -> Int
countAcceptableCombs ws part =
    goW part "in"
  where
    goW :: Part -> WorkflowName -> Int
    goW p "A" = product . fmap (size . snd) $ p
    goW _ "R" = 0
    goW p w =
          lookup w ws
        & fromMaybe (error $ "Unknown workflow:" ++ show w)
        & goR p

    goR :: Part -> [Rule] -> Int
    goR p [(Always,nextW)]           = goW p nextW
    goR p ((Less    cat x,nextW):rs) = sumHitAndNohit p cat nextW rs (Range 0 x) (Range x (maxBound `div` 2))
    goR p ((Greater cat x,nextW):rs) = sumHitAndNohit p cat nextW rs (Range (x+1) (maxBound `div` 2)) (Range 0 (x+1))
    goR _ _                          = error "Unexpected rule pattern"

    sumHitAndNohit :: Part -> Cat -> WorkflowName -> [Rule] -> RangeInt -> RangeInt -> Int
    sumHitAndNohit p cat nextW rs hit nohit =
        lookup cat p & fromJust & \px ->
              (px `intersection` hit   & maybe (goR p rs) (\npx -> goW (insert (cat,npx) p) nextW))
            + (px `intersection` nohit & maybe 0          (\npx -> goR (insert (cat,npx) p) rs))

solve1 :: String -> Int
solve1 s =
    let (workflows,parts) =  parseOrDie workflowsAndParts s
    in   parts
       & filter ((0 /=) . countAcceptableCombs workflows)
       & concatMap (fmap $ offset . snd)
       & sum

solve2 :: String -> Int
solve2 s =
    let (workflows,_) =  parseOrDie workflowsAndParts s
        r = Range 1 4000
    in countAcceptableCombs workflows (mkPart r r r r)
