module Terminal
    ( module System.Console.ANSI
    , pattern KeyEsc
    , pattern KeyUp
    , pattern KeyDown
    , pattern KeyRight
    , pattern KeyLeft
    , pattern KeyDel
    , pattern KeyBackspace
    , pattern KeyEnter
    , withTerminal
    , getKey
    ) where

import           Data.String         (IsString)
import           GHC.IO.Handle       (hGetBuffering, hGetEcho)
import           System.Console.ANSI (clearScreen, getTerminalSize, hideCursor,
                                      setCursorPosition, showCursor)
import           System.IO           (BufferMode (..), hReady, hSetBuffering,
                                      hSetEcho, stdin)

import           Imports             (finally)

pattern KeyEsc, KeyUp, KeyDown, KeyRight, KeyLeft, KeyDel, KeyBackspace, KeyEnter :: (Eq a, IsString a) => a
pattern KeyEsc          = "\ESC"
pattern KeyUp           = "\ESC[A"
pattern KeyDown         = "\ESC[B"
pattern KeyRight        = "\ESC[C"
pattern KeyLeft         = "\ESC[D"
pattern KeyDel          = "\ESC[3~"
pattern KeyBackspace    = "\DEL"
pattern KeyEnter        = "\n"

withTerminal :: IO a -> IO a
withTerminal a =
    before >>= (a `finally`) . after
  where
    before = do
        s <- (,) <$> hGetBuffering stdin <*> hGetEcho stdin
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        hideCursor
        pure s

    after (b,e) = do
        hSetBuffering stdin b
        hSetEcho stdin e
        showCursor

getKey :: IO [Char]
getKey =
    reverse <$> getKey' ""
  where
    getKey' chars = do
        char <- getChar
        r <- hReady stdin
        (if r then getKey' else return) (char:chars)

-- Y23.D14
-- visualize :: IO ()
-- visualize = do
--     i0 <- (:[]) . lines <$> readInput "Y23.D99"

--     clearScreen
--     setCursorPosition 0 0

--     withTerminal $ fix1 i0 \loop xxs@(x:xs) -> do
--         -- deepseq x

--         setCursorPosition 0 0

--         getTerminalSize >>= maybe (pure ()) (\(_r,c) -> do
--             putStrLn $ replicate c ' '
--             putStrLn $ replicate c ' ')

--         setCursorPosition 0 0
--         putStrLn $ "Weights: " ++ show (fmap countWeight . reverse $ xxs)

--         setCursorPosition 3 0
--         mapM_ (putStrLnColored . intersperse ' ') x

--         k <- getKey

--         case k of
--             KeyEsc   -> pure ()
--             KeyUp    -> loop $ moveN x : xxs
--             KeyDown  -> loop $ moveS x : xxs
--             KeyRight -> loop $ moveE x : xxs
--             KeyLeft  -> loop $ moveW x : xxs
--             "f"      -> loop $ (moveE . moveS . moveW . moveN) x : xxs
--             " "      -> loop $ if null xs then xxs else xs
--             _        -> loop xxs
--   where
--     putStrLnColored :: String -> IO ()
--     putStrLnColored = putStrLn . replaceAll "O" "\ESC[31mO\ESC[0m"

