{-# LANGUAGE TemplateHaskell #-}
module MainExeTH
    ( solversFromImportsAndSources
    ) where

import qualified Data.Map.Strict     as M
import           Language.Haskell.TH
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     (takeBaseName, takeDirectory, takeFileName,
                                      (</>))

import qualified Y15.D06

import           Imports

solversFromImportsAndSources :: Q Exp
solversFromImportsAndSources = do
    files <- runIO listDaysSources
    [| $(ListE <$> forM files solversFromFile) |]

listDaysSources :: IO [FilePath]
listDaysSources =
    sort . filter (isPrefixOf "D" . takeFileName) <$> listFilesRecursively "src"

solversFromFile :: FilePath -> Q Exp
solversFromFile modFile = do
    group2solverNames <- runIO $ solversNames modFile

    let modName = (takeFileName . takeDirectory $ modFile) ++"." ++ takeBaseName modFile

    -- TODO report missing imports (enumerate and compare against solverNames)

    let callF :: String -> Q Exp
        callF solverName = do
            let modSolverName = modName ++ "." ++ solverName
            adapterForSolver modSolverName

        solverGroup :: String -> [String] -> Q Exp
        solverGroup groupName solverNames =
            [|( $(return $ LitE (StringL (modName ++ groupName)))
              , $(ListE <$> forM solverNames callF)
              ) |]

    -- [("Y15.D01", [id . Y15.D01.solve1, return . Y15.D01.solve2])]
    [| $(ListE <$> forM (M.toList group2solverNames) (uncurry solverGroup) ) |]

adapterForSolver :: String -> Q Exp
adapterForSolver modSolverName = do
    solverName <- lookupValueName modSolverName
        <&> fromMaybe (error $ "Couldn't lookup solver: " ++ show modSolverName)

    VarI _ t _ <- reify solverName
    return $ applyAdapter t modSolverName

applyAdapter :: Type -> String -> Exp
applyAdapter t = case t of
    -- (a -> b)
    AppT (AppT ArrowT (ConT a)) (ConT b) ->
        if | (a,b) == (''String, ''Int)              -> return_show_
           | (a,b) == (''String, ''Y15.D06.Solution) -> return_show_ -- TODO resolve type alias
           | (a,b) == (''String, ''String)           -> return_
           | otherwise                               -> die
    -- (a -> m b)
    AppT (AppT ArrowT (ConT a)) (AppT (ConT m) (ConT b)) ->
        if | (a,m,b) == (''String, ''IO, ''Int)              -> fmap_show_
           | (a,m,b) == (''String, ''IO, ''Y15.D06.Solution) -> fmap_show_ -- TODO resolve type alias
           | otherwise                                       -> die
    _ -> die
  where
    return_show_ x = InfixE (Just $ return_ "show") (vare ".") (jvare x)
    fmap_show_ x   = InfixE (Just (AppE (vare "fmap") (vare "show"))) (vare ".") (jvare x)
    return_ x      = InfixE (jvare "return") (vare ".") (jvare x)
    vare           = VarE . mkName
    jvare          = Just . vare
    die            = error $ "Couldn't make an adapter for solver of type: " ++ show t

solversNames :: FilePath -> IO (Map String [String])
solversNames modFile = do
    t <- readFile modFile
    return $
          lines t
        & filter (\l -> "solve1" `isPrefixOf` l || "solve2" `isPrefixOf` l)
        & fmap (head . words)
        & filter (all isAlphaNum)
        & fmap (\x -> (drop (length ("solve1"::String)) x, [x]))
        & nub
        & M.fromListWith (++)
        & M.map sort

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively rootDir = do
    names <- listDirectory rootDir <&> filter (not . isPrefixOf ".")

    concat <$> forM names (\name -> do
        let path = rootDir </> name
        doesDirectoryExist path >>= bool
            (return [path])
            (listFilesRecursively  path))
