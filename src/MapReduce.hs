module MapReduce (
  Mapper,
  Reducer,
  mapReduce
  ) where

import qualified Data.Map as Map

-- Mapper takes input type and produces list of key-value pairs
type Mapper inputKey inputValue reduceKey intermediateValue
  = (inputKey, inputValue) -> [(reduceKey, intermediateValue)]

-- Reducer takes key, list of values, and folds those values into single value
type Reducer reduceKey reduceValue = reduceKey -> [reduceValue] -> reduceValue

mapReduce :: (Ord reduceKey) => Mapper inputKey inputValue reduceKey reduceValue
                       -> Reducer reduceKey reduceValue
                       -> [(inputKey, inputValue)] -> [(reduceKey, reduceValue)]
mapReduce mapper reducer = reduce reducer . shuffleKeys . concatMap (map valToList . mapper)
  where valToList (key, value) = (key, [value])
        shuffleKeys = Map.fromListWith (++)
        reduce reducer = Map.toList . Map.mapWithKey reducer
