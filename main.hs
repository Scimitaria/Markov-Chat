module Main where
import Models.LN
import Models.LNChar
import Data.Maybe
import System.IO
import System.Console.GetOpt

data Flag = N Integer | Char | Help  deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['n'] ["n"]             (ReqArg N "<num>")    "Specifies key size."
          , Option ['c'] ["char"]          (NoArg Char)          "Selects the LNChar model."
          , Option ['h'] ["help"]          (NoArg Help)          "Print usage information and exit."
          ]

--get key size; 1 by default
getN :: [Flag] -> Int
getN [] = 1
getN (N n:_) = 
    case readMaybe s of
        Just size -> size
        _ -> 1
getSize (_:fs) = getSize fs

--TODO: add i, default random

main :: IO()
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    if (Help `elem` flags) || (not $ null errors)
    then putStrLn $ usageInfo "Markov-Chat [options] [filename]." options
    else
        let n = getN flags
        if Char `elem` flags then
            --let model = buildModel
            --generateResponseLn n model i
        else
            --let model = buildModel
            --generateResponseLn n model i            
