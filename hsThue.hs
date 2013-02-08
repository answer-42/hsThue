module Main where

import System.Environment (getArgs)

import Data.List.Split (splitOn)

delim = "::=" :: String
outp  = '~'   :: Char

data Action = Output | Subst
  deriving (Eq,Show)

splitAtFirst :: Eq a => [a] -> [a] -> ([a],[a]) 
splitAtFirst _ _ = ([],[])

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

main :: IO ()
main = do
  [sw,fn] <- getArgs
  f <- readFile fn
  let rB = mkRuleBase f  -- Rulebase
      iS = mkInitState f -- Initial state 
  print rB
  print iS

  -- void $ run

