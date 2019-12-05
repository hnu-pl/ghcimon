-- vim: ts=2: sw=2: expandtab: ai:
module Lib where

import Data.Char
import Data.List
import Text.Printf
import Network.URI.Encode (encode, decode)

-- add echo CMD to .ghci
inputFilter :: String -> String
inputFilter = unlines . addCMDs . ([]:) . lines

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
                       0   ->   "</a></div>":[]
                       1   ->   atag :  addInTags ls
                       _   ->  concat ["</a></div>" , atag ] : addInTags ls
  | otherwise  = l : addInTags ls
      where  atag = concat ["<div class='input' id='",(tail (dropWhile (/= '#' )  $ head $ lines l)) , "><a href = 'test-out-raw.html",(dropWhile (/= '#' )  $ head $ lines l) ,", class='input' target='out'>"]



inputStyle = unlines
  [ "<style type='text/css'>"
  , "a.input { all: unset; }"
  , "a:focus.input { background-color: yellow; }"
  , "div.input:target { background-color: #DDDDDD; }"
  , "</style>"
  ]

-- add anchor tag to input html
tagOutputHTML :: String -> String
tagOutputHTML = unlines . addOutTags . lines

outputStyle = unlines
  [ "<style type='text/css'>"
  , "a.output { all: unset; }"
  , "a:focus.output { background-color: yellow; }"
  , "div.output:target { background-color: #DDDDDD; }"
  , "</style>"
  ]

addOutTags []     = []
addOutTags (l:ls)
  | l=="<PRE>" = outputStyle : l : addOutTags ls
  | hasCMD l   =  case checkCMD l of
                    0 -> "</a></div>":[]
                    1 ->  atag
                        : addOutTags ls
                    n -> concat ["</a></div>" , atag] 
                        : addOutTags ls
  | hasRAW l   = decode (parseRAW l) : addOutTags ls
  | otherwise  = l : addOutTags ls
      where  atag = concat 
                        ["<div id='CMD", 
                        (printf "%05d" (checkCMD l :: Int)) ,
                        "' class='output'><a href='test-in-raw.html",
                        "#", "CMD", (printf "%05d" (checkCMD l :: Int)),
                        "' target='in'>"
                        ]
    
    
hasRAW :: String -> Bool
hasRAW l = "</FONT></B>GHCIMONRAW <B>" `isPrefixOf` l

parseRAW l = "<img alt='picsum' src='https://picsum.photos/700/200'>" -- TODO

hasCMD :: String -> Bool
hasCMD l = "#CMD" `isInfixOf` l 

checkCMD l = read $ filter isNumber (getCMD "#CMD" l) :: Int 

getCMD :: String -> String -> String
getCMD [] l =  l
getCMD s  [] =  [] 
getCMD s@(x:xs)  (y:ys) = if x==y then x:(getCMD xs ys) else getCMD s ys
