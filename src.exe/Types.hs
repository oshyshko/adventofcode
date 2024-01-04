module Types where

type Input                  = String
type Answer                 = Int
type AnswerStr              = String

type DayPrefix              = String                -- "Y15.D01", "Y15.D06VU"
type ModuleName             = String                -- "Y15.D01", "Y15.D06"
type Part1or2               = Int                   -- 1, 2
type Solver                 = Input -> IO AnswerStr -- (return . show . Y15.D01.solve1)

type SourcePath             = FilePath              -- "src/Y15/D01.hs"
type AltSuffix              = String                -- "", "VS"
type SolverFnName           = String                -- "solve1", "solve2VS"
type ModuleNameSolverFnName = String                -- "Y15.D01.solve1"

data Day = Day
    { dayPrefix :: DayPrefix
    , runAlts   :: Bool
    , solvers   :: [Solver]
    }

data RunResult = RunResult
    { output         :: AnswerStr
    , msReal         :: Integer
    , bytesAllocated :: Integer
    , bytesPeak      :: Integer
    , bytesMaxInUse  :: Integer
    } deriving Show
