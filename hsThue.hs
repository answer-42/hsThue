module Main where

import System.Environment (getArgs)

import Data.List.Split (splitOn)

import Control.Monad (void)

{- TODO
 - * Write String Split library
 -    * splitOn :: String -> String -> [String]
 -    * substitute :: String -> String -> String
 -}

delim = "::=" :: String
outp  = '~'   :: Char

data Action = Output | Subst
  deriving (Eq,Show)

data Flag = Debug | NoFlag
  deriving (Eq,Show)

type RuleBase = [(String,String,Action)]

mkRuleBase :: String -> RuleBase
mkRuleBase x = map (addAnn . twoTuple . splitOn delim) $
                filter (not . null) $
                takeWhile (/=delim) $ lines x 
  where twoTuple ::  [String] -> (String,String)
        twoTuple [x,y] = (x,y)
        twoTuple _     = error "Rulebase is not correct -- mkRuleBase"

        addAnn :: (String,String) -> RuleBase
        addAnn (x,y:ys) | y == outp = (x,ys,Output)
                        | otherwise = (x,y:ys,Subst) 

mkInitState :: String -> String
mkInitState = unlines . tail . dropWhile (/=delim) . lines

mkFlag :: String -> Flag
mkFlag x | x == "d"  = Debug
         | x == "n"  = NoFlag 
         | otherwise = error "Unknown Flag -- mkFlag"

runProg :: Flag -> RuleBase -> String
runProg Debug rB iS = f iS False
  where f s d = if d 
                then s
                else -- map subsequences s

main :: IO ()
main = do
  args <- getArgs
  f <- readFile $ if length args == 1 then head args else args !! 1
  let rB = mkRuleBase f  -- Rulebase
      iS = mkInitState f -- Initial state 
      -- Flag
      fl = mkFlag $ if length args == 2 then head args else "n"    

  void $ runProg fl rB iS

