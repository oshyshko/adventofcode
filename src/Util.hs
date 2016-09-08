module Util where

-- See also: Control.Arrow.(&&&)
juxt :: [a -> b] -> a -> [b]
juxt fns x = map (\f -> f x) fns  -- alternative: juxt fns = fns <*> . (:[])
