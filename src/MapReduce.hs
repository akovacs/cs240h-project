module MapReduce (
  Mapper,
  Reducer,
  mapReduce
  ) where

import qualified Data.Map as Map

-- Mapper takes input type and produces list of key-value pairs
type Mapper inputType key value = inputType -> [(key, value)]

-- Reducer takes key, list of values, produces new list of values
type Reducer key value = key -> [value] -> [value]

mapReduce :: Ord key => Mapper inputType key value -> Reducer key value
                     -> [inputType] -> [(key, [value])]
mapReduce mapper reducer = reduce reducer . shuffleKeys . concatMap (map valToList . mapper)
  where valToList (key, value) = (key, [value])
        shuffleKeys = Map.fromListWith (++)
        reduce reducer = Map.toList . Map.mapWithKey reducer
