{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main where

import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (void)

{- TODO
 - * Write T.Text Split library
 -    * splitOn :: T.Text -> T.Text -> [T.Text]
 -    * substitute :: T.Text -> T.Text -> T.Text
 -}

delim = "::=" :: T.Text
outp  = '~'   :: Char

data Action = Output | Subst
  deriving (Eq,Show)

data Flag = Debug | NoFlag
  deriving (Eq,Show)

type RuleBase = (T.Text,T.Text,Action)

mkRuleBase :: T.Text -> [RuleBase]
mkRuleBase x = map (addAnn . twoTuple . T.splitOn delim) $
                filter (not . T.null) $
                takeWhile (/=delim) $ T.lines x 
  where twoTuple ::  [T.Text] -> (T.Text,T.Text)
        twoTuple [x,y] = (x,y)
        twoTuple _     = error "Rulebase is not correct -- mkRuleBase"

        addAnn :: (T.Text,T.Text) -> RuleBase
        addAnn (x,y:ys) | y == outp = (x,ys,Output)
                        | otherwise = (x,y:ys,Subst) 

mkInitState :: T.Text -> T.Text
mkInitState = T.unlines . tail . dropWhile (/=delim) . T.lines

mkFlag :: T.Text -> Flag
mkFlag x | x == "d"  = Debug
         | x == "n"  = NoFlag 
         | otherwise = error "Unknown Flag -- mkFlag"

--runProg :: Flag -> RuleBase -> T.Text
--runProg Debug rB iS = f iS False
--  where f s d = if d 
--                then s
--                else -- map subsequences s

main :: IO ()
main = do
  args <- getArgs
  f <- T.readFile $ if length args == 1 then head args else args !! 1
  let rB = mkRuleBase f  -- Rulebase
      iS = mkInitState f -- Initial state 
      -- Flag
      fl = mkFlag $ if length args == 2 then T.pack $ head args else "n"    

  print rB
  print iS
  print fl
--  void $ runProg fl rB iS

