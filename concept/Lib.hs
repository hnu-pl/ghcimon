-- vim: ts=2: sw=2: expandtab: ai:
module Lib where

import Data.Char
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

addInTags (l:ls)
  | l=="<PRE>" = inputStyle : l : addInTags ls
  -- | hasCMD l   = undefined
  | otherwise  = l : addInTags ls
addInTags []     = []

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

addOutTags (l:ls)
  | l=="<PRE>" = outputStyle : l : addOutTags ls
  -- | hasCMD l   = undefined
  | otherwise  = l : addOutTags ls
addOutTags []     = []


