module Models.L2 where
import Debug.Trace
import System.Random (randomRIO)
import Data.List.Split (splitWhen, splitOn)
import Data.Char

type Key = (String,String)
type Token = String
type Model = [(Key, [Token])]

--get random element of list
randomFrom :: [a] -> IO a
randomFrom lst =
  do index <- randomRIO (0,(length lst)-1)
     return (lst!!index)

--extract the [Token] corresponding to k
extract :: Key -> Model -> Maybe ([Token], Model)
extract k [] = Nothing
extract k ((key,val):model)
  | k == key  = Just (val,model)
  | otherwise = 
      do (vs,model') <- extract k model
         return (vs,(key,val):model')

---add (k,t) to model
addToModel :: Key -> Token -> Model -> Model
addToModel k t m =
  case extract k m of
    Nothing -> (k,[t]):m
    Just (vs,m') -> (k,t:vs):m'

--format output of input.txt
cleanText :: String -> [String]
cleanText text = [map toLower $ filter keep w | w <- splitWhen isExtSpace text, not $ null w]
  where isExtSpace c = c `elem` "-" || isSpace c
        keep c = (isAlpha c) || (isNumber c) || (c == '\'') || (c == '$')

--turn contents of input.txt into a Model
buildModel :: String -> Model
buildModel text = 
  case cleanText text of 
    (t1:t2:rest) -> aux (t1,t2) rest
    _ -> error "Radically insufficient model."
  where aux (t1,t2) [] = []
        aux (t1,t2) (t3:ts) = addToModel (t1,t2) t3 (aux (t2,t3) ts)

--print model in readable format
showModel :: Model -> IO ()
showModel [] = do return ()
showModel ((key,value):model) = do
  putStrLn $ "Key: " ++ (show key) ++ "; Value: " ++ (show value)
  showModel model

--format as sentence
sentencify :: String -> String
sentencify (c:cs) = (toUpper c):(if (last cs) == '.' then cs else cs ++ ".")

--for use instead of random selection
mostProbable :: [Token] -> Token
mostProbable values = snd $ maximum [(length [t | t <- values, t==tok],tok) | tok <- values]

--Level 2 Markov Model
--keys are two words; value is the list of words following the key sequence
generateResponseL2 :: Key -> Model -> Int -> IO String
generateResponseL2 key m i = fmap (sentencify.unwords) (gen key i)
  where gen :: Key -> Int -> IO [String]
        gen (k1,k2) 0 = return [k1,k2]
        gen (k1,k2) i = 
          case lookup (k1,k2) m of
            Nothing -> return $ traceShow "Ran out of words!" []
            Just vals ->
              do --putStrLn $ "keys found: " ++ k1 ++ "," ++ k2
                 --let nt = mostProbable vals
                 nt <- randomFrom vals
                 resp <- gen (k2,nt) (i-1)
                 return $ (k1:resp)

test :: IO ()
test = do
  text <- readFile "input.txt"
  let model = buildModel text
      keys = map fst model
  --showModel model
  key <- randomFrom keys
  generation <- generateResponseL2 key model 7
  print generation
