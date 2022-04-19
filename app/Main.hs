module Main where
import System.Environment
import System.Exit
import Text.Printf
import Parser

-- ::


expr::Parser Double
expr s = case parseAnd term  (parseChar '+') s of
        Just ((num, _) , s') -> case expr s' of
            Just (num', s') -> Just (num + num', s')
            Nothing -> term s          
        Nothing -> case parseAnd term (parseChar '-') s of
            Just ((num, _) , s') -> case expr s' of
                Just (num', s') -> Just (num - num', s')
                Nothing -> term s          
            Nothing -> term s   

term::Parser Double
term s = case parseAnd pow (parseChar '*') s of
        Just ((num, _) , s') -> case term s' of
            Just (num', s') -> Just (num * num', s')
            Nothing -> pow s          
        Nothing -> case parseAnd pow (parseChar '/') s of
            Just ((num, _) , s') -> case term s' of
                Just (0, s') -> Nothing 
                Just (num', s') -> Just (num / num', s')
                Nothing -> pow s         
            Nothing -> pow s         

pow::Parser Double
pow s = case parseAnd fact (parseChar '^') s of
        Just ((num, _) , s') -> case pow s' of
            Just (num', s') -> Just (num ** num', s')
            Nothing -> fact s          
        Nothing -> fact s

fact:: Parser Double
fact s = case parseAnd (parseChar '(') expr s of 
            Just ((_, num), s') -> Just (num, tail s')
            Nothing -> parseDouble s

main :: IO ()
main = do 
    args <- getArgs
    case eraseAll " " (head args) of 
        Just (cleanArg , _)-> case expr cleanArg of
            Just (res ,  "") -> printf "%.2f\n" res
            Just ( _,  _) -> exitWith (ExitFailure 84)
            Nothing -> exitWith (ExitFailure 84)
        Nothing -> exitWith (ExitFailure 84)