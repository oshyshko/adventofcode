{-# LANGUAGE TemplateHaskell #-}
module DaysTH
    ( days
    ) where

import qualified Data.Map.Strict     as M
import           Language.Haskell.TH (Exp(..), Q, Info(..), Type(..), Lit(..))
import qualified Language.Haskell.TH as TH
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     (takeBaseName, takeDirectory, takeFileName,
                                      (</>))

import           Imports
import           Types

-- TODO test has wrong type for solve1/2
-- TODO test doesn't have solve1/2
-- TODO test has no answer -- at compile-time?

-- [| [Day] |]
days :: Q Exp
days = do
    files <- TH.runIO listSources
    [| $(ListE . join <$> mapM extractDays files) |]

-- > extractDays "src/Y15/D01.hs"
-- [| [Day "Y15.D01" Nothing [return . show . Y15.D01.solve1, return . show . Y15.D01.solve2] ] |]
extractDays :: SourcePath -> Q [Exp]
extractDays s = do
    -- TODO report missing imports (enumerate and compare against solversFnNames)
    s2sn <- TH.runIO $ altSuffixToSolverFnNames s

    forM (M.toList s2sn) (uncurry (solverRow moduleName))
  where
    moduleName = (takeFileName . takeDirectory $ s) ++ "." ++ takeBaseName s

solverRow :: ModuleName -> AltSuffix -> [SolverFnName] -> Q Exp
solverRow moduleName altSuffix solverFnNames =
    [| Day
        $(return . LitE . StringL $ moduleName ++ altSuffix)
        $(return $ ConE $ TH.mkName (show (not $ null altSuffix)))
        $(ListE <$> forM solverFnNames (callF moduleName))
        |]

callF :: ModuleName -> SolverFnName -> Q Exp
callF moduleName solverFnName = do
    let msn = moduleName ++ "." ++ solverFnName

    solver <- TH.lookupValueName msn
        <&> fromMaybe (error $ "Did you forget to add to 'Days.hs' line: import qualified " ++ moduleName)

    VarI _ t _ <- TH.reify solver

    return $ applyAdapter t msn

applyAdapter :: Type -> ModuleNameSolverFnName -> Exp
applyAdapter t msn = case t of
    -- (a -> b)
    AppT        (AppT ArrowT (ConT a)) (ConT b)
        | (a,b) == (''String, ''Int)            -> return_show_ msn
        | (a,b) == (''String, ''String)         -> return_ msn
        | otherwise                             -> die
    ForallT _ _ (AppT (AppT ArrowT (ConT a)) (ConT b))
        | (a,b) == (''String, ''Int)            -> return_show_ msn
        | (a,b) == (''String, ''String)         -> return_ msn
        | otherwise                             -> die
    -- (a -> m b)
    AppT         (AppT ArrowT (ConT a)) (AppT (ConT m) (ConT b))
        | (a,m,b) == (''String, ''IO, ''Int)    -> fmap_show_ msn
        | otherwise                             -> die
    ForallT _ _ (AppT (AppT ArrowT (ConT a)) (AppT (ConT m) (ConT b)))
        | (a,m,b) == (''String, ''IO, ''Int)    -> fmap_show_ msn
        | otherwise                             -> die
    -- unknown
    _ -> dieTop
  where
    return_show_ x = InfixE (Just $ return_ "show") (vare ".") (jvare x)
    fmap_show_ x   = InfixE (Just (AppE (vare "fmap") (vare "show"))) (vare ".") (jvare x)
    return_ x      = InfixE (jvare "return") (vare ".") (jvare x)
    vare           = VarE . TH.mkName
    jvare          = Just . vare
    die            = error $ "Couldn't make an adapter for solver " <> show msn <> " of type: " ++ show t
    dieTop         = error $ "Couldn't make an adapter for solver (via top) " <> show msn <> " of type: " ++ show t

-- > altSuffixToSolverFnNames "src/Y15/D06.hs"
-- fromList [("",["solve1","solve2"]),("AI",["solve1AI","solve2AI"]), ...]
altSuffixToSolverFnNames :: SourcePath -> IO (Map AltSuffix [SolverFnName])
altSuffixToSolverFnNames p =
    readFile p <&>
    --   M.elems
      M.map sort
    . M.fromListWith (++)
    . fmap (\x -> (,[x]) . drop (length ("solveX" :: String)) $ x)
    . nub
    . filter (all isAlphaNum)
    . fmap (head . words)
    . filter (\l -> "solve1" `isPrefixOf` l || "solve2" `isPrefixOf` l)
    . lines

-- ["src/Y15/D01.hs","src/Y15/D02.hs","src/Y15/D03.hs","src/Y15/D04.hs", ...]
listSources :: IO [SourcePath]
listSources =
        listFilesRecursively "src"
    <&> filter (\p ->
            let f = takeFileName p
            in isPrefixOf "D" f && isSuffixOf ".hs" f)

-- > listFilesRecursively "src"
-- ["src/Imports.hs","src/Util.hs","src/Y15/D01.hs","src/Y15/D02.hs", ...]
--
listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively rootDir = do
    files <- listDirectory rootDir <&> filter (not . isPrefixOf ".")

    sort . concat <$> forM files (\f -> do
        let path = rootDir </> f
        doesDirectoryExist path >>= bool
            (return [path])
            (listFilesRecursively  path))
