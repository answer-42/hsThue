{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main where

import System.Environment (getArgs)

import Data.Text (Text, splitOn, null, lines, unlines, cons, 
                  uncons, strip, concat, count, pack,replace)
import Data.Text.IO (putStr,putStrLn,readFile)

import Prelude hiding (null, lines, unlines, readFile,
                       cons, concat, putStr,putStrLn)

import Control.Monad (when,foldM)

{- TODO
 - * add proper testing
 -}

{-
"THE BEER-WARE LICENSE" (Revision 42):
<sebastian.benque@gmail.com> wrote this file. As long as you retain this notice you
can do whatever you want with this stuff. If we meet some day, and you think
this stuff is worth it, you can buy me a beer in return Sebastian Benque
-}

delim = "::=" :: Text
outp  = '~'   :: Char

data Action = Output | Subst
  deriving (Eq,Show)

data Flag = Debug | NoFlag
  deriving (Eq,Show)

type RuleBase = (Text,Text,Action)

mkRuleBase :: Text -> [RuleBase]
mkRuleBase x = map (addAnn . twoTuple . splitOn delim) $
                filter (not . null) $
                takeWhile (/=delim) $ lines x 
  where twoTuple ::  [Text] -> (Text,Text)
        twoTuple [x,y] = (x,y)
        twoTuple _     = error "Rulebase is not correct -- mkRuleBase"

        addAnn :: (Text,Text) -> RuleBase
        addAnn (x,uncons -> Just (y,ys)) | y == outp = (x,ys,Output)
                                           | otherwise = (x,cons y ys,Subst) 

mkInitState :: Text -> Text
mkInitState = strip . unlines . tail . dropWhile (/=delim) . lines

mkFlag :: Text -> Flag
mkFlag x | x == "d"  = Debug
         | x == "n"  = NoFlag 
         | otherwise = error "Unknown Flag -- mkFlag"

runProg :: Flag -> [RuleBase] -> Text -> IO Text
runProg  fl rB = f
  where f iS = let g a (x,y,s) = do when (fl == Debug)
                                         $ putStr "DEBUG: " >> putStrLn iS
                                    if s == Output 
                                    then putStrLn (concat $ replicate (count x a) y)
                                         >> return (concat $ splitOn x a)
                                    else return $ replace x y a
                in do iS' <- foldM g iS rB
                      if iS' == iS then return iS' else f iS' 

main :: IO ()
main = do
  args <- getArgs
  f <- readFile $ if length args == 1 then head args 
                  else if length args == 2 then args !! 1
                  else error "Argument error -- main"
  let rB = mkRuleBase f  -- Rulebase
      iS = mkInitState f -- Initial state 
      -- Flag
      fl = mkFlag $ if length args == 2 then pack $ head args else "n"    

  runProg fl rB iS >>= putStrLn

