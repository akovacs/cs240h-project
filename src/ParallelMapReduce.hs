module ParallelMapReduce (
  Mapper,
  Reducer,
  mapReduce
  ) where

import qualified Data.Map as Map
import Control.Parallel (pseq)
import Control.Parallel.Strategies (parList, rdeepseq, using, NFData)

-- Mapper takes input tuple and produces list of key-value pairs
type Mapper inputKey inputValue reduceKey intermediateValue
  = (inputKey, inputValue) -> [(reduceKey, intermediateValue)]

-- Reducer takes key, list of values, and folds those values into single value
type Reducer reduceKey reduceValue = reduceKey -> [reduceValue] -> reduceValue


-- Define parallel map method
parallelMap function list = map function list `using` parList rdeepseq


-- Single-node mapReduce implementation
mapReduce :: (NFData inputKey, NFData inputValue, NFData reduceKey, Ord reduceKey, NFData reduceValue)
          => Mapper inputKey inputValue reduceKey reduceValue
          -> Reducer reduceKey reduceValue
          -> Map.Map inputKey inputValue
          -> Map.Map reduceKey reduceValue
mapReduce mapper reducer = reduce reducer . groupByKey . asList . mapInput mapper
  where mapInput mapper = concat . parallelMap mapper . Map.toList
        asList keyValuePairs = [(key, [value]) | (key, value) <- keyValuePairs]
        groupByKey = Map.fromListWith (++)
        reduce reducer = Map.mapWithKey reducer
