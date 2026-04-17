import Debug.Trace
import System.Random (randomRIO)
import Data.List.Split (splitWhen, splitOn)
import Data.Char

type Key = String
type Token = String
type Model = [(Key, [Token])]

randomFrom :: [a] -> IO a
randomFrom lst =
  do index <- randomRIO (0,length lst-1)
     return (lst!!index)

extract :: Key -> Model -> Maybe ([Token], Model)
extract k [] = Nothing
extract k ((key,val):model)
  | k == key  = Just (val,model)
  | otherwise = 
      do (vs,model') <- extract k model
         return (vs,(key,val):model')

addToModel :: Key -> Token -> Model -> Model
addToModel k t m =
  case extract k m of
    Nothing -> (k,[t]):m
    Just (vs,m') -> (k,t:vs):m'

cleanText text = [map toLower $ filter isAlpha w | w <- splitWhen isExtSpace text, not $ null w]
  where isExtSpace c = c `elem` "-" || isSpace c

buildModel :: String -> Model
buildModel text = aux $ cleanText text
  where aux [] = []
        aux [t] = []
        aux (t1:t2:ts) = addToModel t1 t2 (aux (t2:ts))

generateResponse :: Key -> Model -> Int -> IO String
generateResponse k m i = fmap unwords (aux k i)
  where aux :: Key -> Int -> IO [String]
        aux k 0 = return [k]
        aux k i = 
          case lookup k m of
            Nothing -> return $ traceShow "Ran out of words!" []
            Just lst -> 
              do nt <- randomFrom lst
                 resp <- aux nt (i-1)
                 return $ k:resp
