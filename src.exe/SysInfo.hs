module SysInfo
       ( getSysInfo
       , SysInfo(..)
       ) where

import qualified Data.Version      as V
import qualified System.Info       as SI
import           System.Process    (readProcessWithExitCode)

import Imports

data SysInfo = SysInfo
    { osName       :: Maybe String
    , osArch       :: Maybe String
    , osVersion    :: Maybe String
    , hwModel      :: Maybe String
    , cpuModel     :: Maybe String
    , cpuCores     :: Maybe Int
    , ramTotal     :: Maybe Int
    , ramSpeed     :: Maybe Int
    , compiler     :: Maybe String
    , compilerArch :: Maybe String
    }

mkEmptySysInfo :: SysInfo
mkEmptySysInfo = SysInfo
    { osName       = Just SI.os
    , osArch       = Nothing
    , osVersion    = Nothing
    , hwModel      = Nothing
    , cpuModel     = Nothing
    , cpuCores     = Nothing
    , ramTotal     = Nothing
    , ramSpeed     = Nothing
    , compiler     = Just $ SI.compilerName ++ "-" ++ V.showVersion SI.compilerVersion
    , compilerArch = Just SI.arch
    }

getSysInfo :: IO SysInfo
getSysInfo =
    let os = toLower <$> SI.os in
    if | "darwin" `isPrefixOf` os -> getMac
       | "mingw"  `isPrefixOf` os -> getWin
       | otherwise                -> getLin

-- Platform: darwin, x86_64, v10.15.2, MacBookPro12,1
-- CPU:      Intel(R) Core(TM) i7-5557U CPU @ 3.10GHz, 2 cores
-- RAM:      16.0G @ ?MHz
-- Compiler: ghc-8.6
--
getMac :: IO SysInfo
getMac = do
    osA  <- execM "uname -m"                           <&> (headM . lines =<<)
    osV  <- execM "sw_vers -productVersion"            <&> (headM . lines =<<)
    hwM  <- execM "sysctl -n hw.model"                 <&> (headM . lines =<<)
    cpuC <- execM "sysctl -n hw.physicalcpu"           <&> (readMaybe =<<)
    cpuM <- execM "sysctl -n machdep.cpu.brand_string" <&> (headM . lines =<<)
    ramT <- execM "sysctl -n hw.memsize"               <&> (readMaybe =<<)

    return $ mkEmptySysInfo
        { osArch       = osA
        , osVersion    = osV
        , hwModel      = hwM
        , cpuCores     = cpuC
        , cpuModel     = cpuM
        , ramTotal     = ramT
        , ramSpeed     = Nothing -- TODO find a way to get it
        }

-- Platform: mingw32, x86_64, v10.0.17763, NUC7JYB
-- CPU:      Intel(R) Pentium(R) Silver J5005 CPU @ 1.50GHz, 4 cores
-- RAM:      16.0G @ 2400MHz
-- Compiler: ghc-8.6
--

{-# ANN getWin ("HLint: ignore Use <=<"  :: String) #-}
{-# ANN getWin ("HLint: ignore Use fmap" :: String) #-}
getWin :: IO SysInfo
getWin = do
    let trim    = dropWhile isSpace . dropWhileEnd isSpace
        linesRN = map trim . splitOn "\r\n"

    osV  <- execM "wmic os get Version"          <&> (secondM . linesRN =<<)
    hwM  <- execM "wmic baseboard get Product"   <&> (secondM . linesRN =<<)
    cpuC <- execM "wmic CPU get NumberOfCores"   <&> (readMaybe =<<) . (secondM . linesRN =<<)
    cpuM <- execM "wmic CPU get Name"            <&> (secondM . linesRN =<<)
    ramT <- execM "wmic MEMORYCHIP get Capacity" <&> \ramT ->
                sum . map (fromMaybe 0 . readMaybe) <$> (tailM . linesRN =<< ramT)
    ramS <- execM "wmic MEMORYCHIP get Speed"    <&> (readMaybe =<<) . (secondM . linesRN =<<)

    return $ mkEmptySysInfo
        { osArch       = Nothing -- TODO
        , osVersion    = osV
        , hwModel      = hwM
        , cpuCores     = cpuC
        , cpuModel     = cpuM
        -- TODO take into account parsing errors
        , ramTotal     = ramT
        , ramSpeed     = ramS
        }

-- Platform: linux, i386, v4.4.0-142-generic, processor	: 0
-- CPU:      processor	: 0, ? cores
-- RAM:      ? @ ?MHz
-- Compiler: ghc-8.8
--
-- TODO implement
getLin :: IO SysInfo
getLin = do
    osA     <- execM "uname -m" <&> (headM . lines =<<)
    osV     <- execM "uname -r" <&> (headM . lines =<<)

    -- cpuinfo <- execM "cat /proc/cpuinfo"
    -- freeB   <- execM "free -b"

    if | "86" `isInfixOf` (toLower <$> SI.arch) ->
        return $ mkEmptySysInfo
            { osArch       = osA
            , osVersion    = osV
            -- TODO take into account parsing errors
            , hwModel      = Nothing -- TODO
            , cpuCores     = Nothing -- TODO
            , cpuModel     = Nothing -- TODO
            , ramTotal     = Nothing -- TODO
            , ramSpeed     = Nothing -- TODO
            }
        | "arm" `isInfixOf` (toLower <$> SI.arch) ->

         return $ mkEmptySysInfo
             { osArch       = osA
             , osVersion    = osV
             -- TODO take into account parsing errors
             , hwModel      = Nothing -- TODO
             , cpuCores     = Nothing -- TODO
             , cpuModel     = Nothing -- TODO
             , ramTotal     = Nothing -- TODO
             , ramSpeed     = Nothing -- TODO
             }

        | otherwise ->
         return $ mkEmptySysInfo
             { osArch       = osA
             , osVersion    = osV
             , hwModel      = Nothing
             , cpuCores     = Nothing
             , cpuModel     = Nothing
             , ramTotal     = Nothing
             , ramSpeed     = Nothing
             }

-- helper fns
tryOrNothing :: IO a -> IO (Maybe a)
tryOrNothing ioa =
    (Just <$> ioa) `catch`
        (\(_ :: SomeException) -> return Nothing)

execM :: FilePath -> IO (Maybe String)
execM cmd = tryOrNothing $
        (\(_, out, _) -> out)
            <$> readProcessWithExitCode c args ""
    where c:args = words cmd

-- readFileM :: FilePath -> IO (Maybe String)
-- readFileM path = tryOrNothing $ readFile path

headM :: [a] -> Maybe a
headM = \case
    x:_ -> Just x
    _   -> Nothing

secondM :: [a] -> Maybe a
secondM = \case
    _:x:_ -> Just x
    _     -> Nothing

tailM :: [a] -> Maybe [a]
tailM = \case
    _:xs -> Just xs
    _    -> Nothing
