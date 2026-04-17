module Main where
import Models.MarkovLN
import Models.MarkovLNChar
import Text.Read
import Data.Maybe
import System.IO
import System.Random
import System.IO.Unsafe
import System.Environment
import System.Console.GetOpt

data Flag = N String | Size String | LNChar | Help  deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['n'] ["n"]             (ReqArg N    "<num>") "Specifies key size."
          , Option ['s'] ["size"]          (ReqArg Size "<num>") "Specifies output size."
          , Option ['c'] ["char"]          (NoArg LNChar)        "Selects the LNChar model."
          , Option ['h'] ["help"]          (NoArg Help)          "Print usage information and exit."
          ]

--get key size; 1 by default
getN :: [Flag] -> Int
getN [] = 1
getN (N n:_) = 
    case readMaybe n of
        Just n -> n
        _ -> 1
getN (_:fs) = getN fs

--get output size; random(10,50) by default
getSize :: [Flag] -> Int
getSize [] = unsafePerformIO $ getStdRandom (randomR (10,50))
getSize (Size s:_) = 
    case readMaybe s of
        Just s -> s
        _ -> unsafePerformIO $ getStdRandom (randomR (10,50))
getSize (_:fs) = getSize fs

main :: IO()
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    if (Help `elem` flags) || (not $ null errors)
    then putStrLn $ usageInfo "Markov-Chat [options] [filename]." options
    else do
        let n = getN flags
            size = getSize flags
        if LNChar `elem` flags then testNC n size 
        else testN n size          
