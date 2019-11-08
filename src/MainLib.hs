module MainLib where

import           Control.DeepSeq       (NFData, force)
import           Control.Exception     (evaluate)
import           Control.Monad         (void)
import           Data.List             (intercalate, isPrefixOf)
import           Data.List.Split       (splitOn)
import qualified Data.Map.Strict       as M
import           Data.Ratio            (numerator)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Environment    (getArgs)
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)

import           Util

import qualified Y15.D01
import qualified Y15.D02
import qualified Y15.D03
import qualified Y15.D04
import qualified Y15.D05
import qualified Y15.D06
import qualified Y15.D07
import qualified Y15.D08
import qualified Y15.D09
import qualified Y15.D10
import qualified Y15.D11
import qualified Y15.D12
import qualified Y15.D13
import qualified Y15.D14
import qualified Y15.D15
import qualified Y15.D16
import qualified Y15.D17

days :: [(String, [String -> IO String])]
days =
    [ ("Y15.D01",  i2ios   [Y15.D01.solve1,   Y15.D01.solve2])
    , ("Y15.D02",  i2ios   [Y15.D02.solve1,   Y15.D02.solve2])
    , ("Y15.D03",  i2ios   [Y15.D03.solve1,   Y15.D03.solve2])
    , ("Y15.D04",  i2ios   [Y15.D04.solve1,   Y15.D04.solve2])
    , ("Y15.D05",  i2ios   [Y15.D05.solve1,   Y15.D05.solve2])
    , ("Y15.D06MS",i2ios   [Y15.D06.solve1MS, Y15.D06.solve2MS])
    , ("Y15.D06IO",ioi2ios [Y15.D06.solve1IO, Y15.D06.solve2IO])
    , ("Y15.D06ST",i2ios   [Y15.D06.solve1ST, Y15.D06.solve2ST])
    , ("Y15.D07",  i2ios   [Y15.D07.solve1,   Y15.D07.solve2])
    , ("Y15.D08",  i2ios   [Y15.D08.solve1,   Y15.D08.solve2])
    , ("Y15.D09",  i2ios   [Y15.D09.solve1,   Y15.D09.solve2])
    , ("Y15.D10",  i2ios   [Y15.D10.solve1,   Y15.D10.solve2])
    , ("Y15.D11",  s2ios   [Y15.D11.solve1,   Y15.D11.solve2])
    , ("Y15.D12",  i2ios   [Y15.D12.solve1,   Y15.D12.solve2])
    , ("Y15.D13",  i2ios   [Y15.D13.solve1,   Y15.D13.solve2])
    , ("Y15.D14",  i2ios   [Y15.D14.solve1,   Y15.D14.solve2])
    , ("Y15.D15",  i2ios   [Y15.D15.solve1,   Y15.D15.solve2])
    , ("Y15.D16",  i2ios   [Y15.D16.solve1,   Y15.D16.solve2])
    , ("Y15.D17",  i2ios   [Y15.D17.solve1,   Y15.D17.solve2])
    ]
  where s2ios :: [a -> b] -> [a -> IO b]
        s2ios   = fmap (return .)
        i2ios :: [a -> Int] -> [a -> IO String]
        i2ios   = fmap ((return . show) .)
        ioi2ios :: [a -> IO Int] -> [a -> IO String]
        ioi2ios = fmap (fmap show .)

-- # day     answer-1  answer-2
-- Y15.D01   138       1771
-- Y15.D02   1586300   3737498
{-# ANN parseAnswers ("HLint: ignore Use map once" :: String) #-}
parseAnswers :: String -> M.Map String [String]
parseAnswers =
    M.fromList
    . map (\case
            day:answers -> (day, answers)
            x -> error $ "Couldn't parse answers: " ++ show x)
    . map (filter (/= "") . splitOn " ")
    . filter (not . isPrefixOf "#")
    . lines

main :: IO ()
main = do
    -- select day(s)
    args <- getArgs
    let daysPred = case args of
            []  -> const True
            [x] -> (x `isPrefixOf`)
            _   -> error $ "Don't know how to interpret args: " ++ show args

    let daysSelected = filter (daysPred . fst) days
    if null daysSelected
        then error $ "Couldn't find day " ++ (show . head) args
            ++ ".\nAvailable days are: " ++ intercalate ", " (map fst days) ++ ".\n"
        else putStrLn $ "Running "
            ++ (show . length $ daysSelected)
            ++ " days (out of " ++ (show . length $ days) ++ ")"

    -- answers.txt
    let answersPath = "res/answers.txt"
    day2snwers <- parseAnswers <$> readFile answersPath

    -- run
    mapM_
        (\ (name, solvers) -> do
            input <- readInput name
            printf "%-9s --> " name
            hFlush stdout
            at <- mapM (\solve -> timeOf (solve input)) solvers
            let actual   = map fst at
                actualMs = map (show . snd) at
            printf "%-18s -- %s ms%s\n"
                (intercalate ", " actual)
                (intercalate ", " actualMs)
                (case M.lookup name day2snwers of
                    Nothing -> " <-- couldn't find entry " ++ show name ++ " in " ++ show answersPath
                    Just expected ->
                        if expected /= actual
                        then " <-- got wrong answers, expected: " ++ intercalate ", " expected
                        else ""))
        daysSelected

timeOf :: NFData a => IO a -> IO (a, Integer)
timeOf ioa = do
    start <- timeInMillis
    a <- force <$> ioa
    void $ evaluate a
    end <- timeInMillis
    pure (a, end - start)
  where
    timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
    timeInMillis = (`div` 1000) . fromIntegral <$> timeInMicros
