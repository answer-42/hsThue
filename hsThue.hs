{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main where

import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.IO as T

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
        addAnn (x,T.uncons -> Just (y,ys)) | y == outp = (x,ys,Output)
                                           | otherwise = (x,T.cons y ys,Subst) 

mkInitState :: T.Text -> T.Text
mkInitState = T.strip . T.unlines . tail . dropWhile (/=delim) . T.lines

mkFlag :: T.Text -> Flag
mkFlag x | x == "d"  = Debug
         | x == "n"  = NoFlag 
         | otherwise = error "Unknown Flag -- mkFlag"

runProg :: Flag -> [RuleBase] -> T.Text -> IO T.Text
runProg  fl rB = f
  where f iS = let g a (x,y,s) = do when (fl == Debug)
                                         $ T.putStr "DEBUG: " >> T.putStrLn iS
                                    if s == Output 
                                    then T.putStrLn (T.concat $ replicate (T.count x a) y)
                                         >> return (T.concat $ T.splitOn x a)
                                    else return $ T.replace x y a
                in do iS' <- foldM g iS rB
                      if iS' == iS then return iS' else f iS' 

main :: IO ()
main = do
  args <- getArgs
  f <- T.readFile $ if length args == 1 then head args 
                    else if length args == 2 then args !! 1
                    else error "Argument error -- main"
  let rB = mkRuleBase f  -- Rulebase
      iS = mkInitState f -- Initial state 
      -- Flag
      fl = mkFlag $ if length args == 2 then T.pack $ head args else "n"    

  runProg fl rB iS >>= T.putStrLn

