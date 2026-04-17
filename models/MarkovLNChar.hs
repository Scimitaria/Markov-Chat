module Models.MarkovLNChar where
import Debug.Trace
import System.Random (randomRIO)
import Data.List.Split (splitWhen, splitOn)
import Data.Char

type Key = [Char]
type Token = Char
type Chain = [(Key, [Token])]
type Model = (Int,Chain)

--get random element of list
randomFrom :: [a] -> IO a
randomFrom lst =
  do index <- randomRIO (0,length lst-1)
     return (lst!!index)

--extract the [Token] corresponding to k
extract :: Key -> Chain -> Maybe ([Token], Chain)
extract k [] = Nothing
extract k ((key,val):chain)
  | k == key  = Just (val,chain)
  | otherwise = 
      do (vs,chain') <- extract k chain
         return (vs,(key,val):chain')

---add (k,t) to chain
addToChain :: Key -> Token -> Chain -> Chain
addToChain k t c =
  case extract k c of
    Nothing -> (k,[t]):c
    Just (vs,c') -> (k,t:vs):c'

--format output of input.txt
cleanText :: String -> [Char]
cleanText text = collapseSpace $ map cleanSpace text
  where cleanSpace c = if (c `elem` "-" || isSpace c) then ' ' else c
        collapseSpace (' ':' ':rest) = collapseSpace (' ':rest)
        collapseSpace (c:cs) = c:(collapseSpace cs)
        collapseSpace [] = []

buildModel :: String -> Int -> Model
buildModel text n = 
  case (length text > n, cleanText text) of 
    (True, words) -> aux (take n words) (drop n words) []
    _ -> error "Radically insufficient model."
  where aux :: Key -> [Char] -> Chain -> Model
        aux key [] chain = (n, chain)
        aux key (t:ts) chain = 
           let nextKey = tail key ++ [t]
               newChain = addToChain key t chain
           in aux nextKey ts newChain

--print chain in readable format
showChain :: Chain -> IO ()
showChain [] = do return ()
showChain ((key,value):chain) = do
  putStrLn $ "Key: " ++ (show key) ++ "; Value: " ++ (show value)
  showChain chain

--format as sentence
sentencify ::  String -> String
sentencify (c:cs) = (toUpper c):(if (last cs) == '.' then cs else cs ++ ".")

--Level n Markov Model
--keys are n chars; value is the list of chars following the key sequence
generateResponseLn :: Key -> Model -> Int -> IO String
generateResponseLn keys (n,c) i = fmap sentencify (gen keys i [])
  where gen :: Key -> Int -> [Char] -> IO [Char]
        gen keys 0 resp = return ((reverse resp) ++ keys)
        gen keys@(k:ks) i resp = 
          case lookup keys c of
            Nothing -> return $ traceShow "Ran out of words!" []
            Just vals -> 
              do nt <- randomFrom vals
                 gen (ks ++ [nt]) (i-1) (k:resp)

testNC :: Int -> Int -> IO ()
testNC n len = do
  text <- readFile "input.txt"
  let model = buildModel text n
      keys = filter (isUpper.head) $ map fst $ snd model
  key <- randomFrom keys
  generation <- generateResponseLn key model len
  print generation
