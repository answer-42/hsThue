module Main where

import System.Environment (getArgs)

import Data.List.Split (splitOn)

import Control.Monad (void)

delim = "::=" :: String
outp  = '~'   :: Char

data Action = Output | Subst
  deriving (Eq,Show)

data Flag = Debug | LeftE | RightE
  deriving (Eq,Show)

mkRuleBase :: String -> [(String,String,Action)]
mkRuleBase x = map (addAnn . twoTuple . splitOn delim) $
                filter (not . null) $
                takeWhile (/=delim) $ lines x 
  where twoTuple ::  [String] -> (String,String)
        twoTuple [x,y] = (x,y)
        twoTuple _     = error "Rulebase is not correct -- mkRuleBase"

        addAnn :: (String,String) -> (String,String,Action)
        addAnn (x,y:ys) | y == outp = (x,ys,Output)
                        | otherwise = (x,y:ys,Subst) 

mkInitState :: String -> String
mkInitState = unlines . tail . dropWhile (/=delim) . lines

mkFlag :: String -> Flag
mkFlag x | x == "d"  = Debug
         | x == "r"  = RightE
         | x == "l"  = LeftE
         | otherwise = error "Unknown Flag -- mkFlag"

-- runProg :: String

main :: IO ()
main = do
  [sw,fn] <- getArgs
  f <- readFile fn
  let rB = mkRuleBase f  -- Rulebase
      iS = mkInitState f -- Initial state 
      fl = mkFlag sw     -- Flag
  print rB
  print iS
  print fl

--  void $ runProg fl rB iS

