-- vim: ts=2: sw=2: expandtab: ai:
module Lib where

import Data.Char
import Data.List
import Text.Printf

-- add echo CMD to .ghci
inputFilter :: String -> String
inputFilter = unlines . addCMDs . lines

addCMDs = addcmds 1

addcmds :: Int -> [String] -> [String]
addcmds n (l:ls) | all isSpace l = printf ":!echo '#CMD%05d'" n : addcmds (n+1) ls
                 | otherwise     = l : addcmds n ls
addcmds _ [] = [printf ":!echo '#CMD%05d'" (0::Int)] -- end with #CMD00000

-- add anchor tag to input html
tagInputHTML :: String -> String
tagInputHTML = unlines . addInTags . lines

addInTags []     = []
addInTags (l:ls)
  | l=="<PRE>"   = inputStyle : l : addInTags ls
  | hasCMD l     =  case checkCMD l of
                       0   ->   "</a>":[]
                       1   ->   concat ["<a href = 'test-out-raw.html" , (dropWhile (/= '#' )  $ head $ lines l) , " class='input' target='out'>"] :  addInTags ls
                       _   ->   concat ["</a><a href = 'test-out-raw.html", (dropWhile (/= '#' )  $ head $ lines l) , " class='input' target='out'>"]: addInTags ls
  | otherwise  = l : addInTags ls




inputStyle = unlines
  [ "<style type='text/css'>"
  , "a.input { all: unset; }"
  , "a:focus.input { background-color: yellow; }"
  , "</style>"
  ]

-- add anchor tag to input html
tagOutputHTML :: String -> String
tagOutputHTML = unlines . addOutTags . lines

outputStyle = unlines
  [ "<style type='text/css'>"
  , ".output:target { background-color: #DDDDDD; }"
  , "</style>"
  ]

addOutTags []     = []
addOutTags (l:ls)
  | l=="<PRE>" = outputStyle : l : addOutTags ls
  | hasCMD l     =  case checkCMD l of
                      0 -> "</div>":[]
                      1 ->  concat ["<div id='CMD"       , (printf "%05d" (1 :: Int)) ,"' class='output'>"] : addOutTags ls
                      n ->  concat ["</div><div id='CMD" , (printf "%05d" (n ::Int))  ,"' class='output'>"] : addOutTags ls
  | otherwise  = l : addOutTags ls



hasCMD :: String -> Bool
hasCMD l = "#CMD" `isInfixOf` l 

checkCMD l = read $ filter isNumber (getCMD "#CMD" l) :: Int 

getCMD :: String -> String -> String
getCMD [] l =  l
getCMD s  [] =  [] 
getCMD s@(x:xs)  (y:ys) = if x==y then x:(getCMD xs ys) else getCMD s ys