module Types where

type Input                  = String
type Answer                 = Int
type AnswerStr              = String

type DayPrefix              = String                -- "Y15.D01", "Y15.D06VU"
type ModuleName             = String                -- "Y15.D01", "Y15.D06"
type SolverIndex            = Int                   -- 0, 1
type Solver                 = Input -> IO AnswerStr -- (return . show . Y15.D01.solve1)

type SourcePath             = FilePath              -- "src/Y15/D01.hs"
type BenchSuffix            = String                -- "", "VS"
type SolverFnName           = String                -- "solve1", "solve2VS"
type ModuleNameSolverFnName = String                -- "Y15.D01.solve1"

data Day = Day
    { dayPrefix :: DayPrefix
    , benchmark :: Bool
    , solvers   :: [Solver]
    }

instance Show Day where
    show Day{dayPrefix,benchmark,solvers} =
        "Day"
            <> "{ dayPrefix="   <> show dayPrefix
            <> ", benchmark="   <> show benchmark
            <> ", solvers="     <> show (length solvers)
            <> "}"

data ExecResult = ExecResult
    { output         :: AnswerStr
    , msReal         :: Integer
    , bytesAllocated :: Maybe Integer
    , bytesPeak      :: Maybe Integer
    } deriving (Show)
