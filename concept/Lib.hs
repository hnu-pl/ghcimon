-- vim: ts=2: sw=2: expandtab: ai:
module Lib where

import Data.Char
import Data.List
import Text.Printf
import Network.URI.Encode (encode, decode)

-- add echo CMD to .ghci
inputFilter :: String -> String
inputFilter = unlines . addCMDs . ([]:) . lines

addCMDs = addcmds True 1

addcmds :: Bool -> Int -> [String] -> [String] -- :{ :}  체크용 Bool 추가 필요 (:{ :} 사이 공백 시 에러
addcmds b _ [] = [printf ":!echo '#CMD%05d'" (0::Int)] -- end with #CMD00000
addcmds b n (l:ls) 
                 | b == False                       = if  ":}" `isInfixOf` l then addcmds True n ls else addcmds False n ls
                 | b == True &&  ":{" `isInfixOf` l = printf ":!echo '#CMD%05d'" n : addcmds False (n+1) ls 
                 | all isSpace l && (not (null ls) && all isSpace (head ls)) 
                                                    = addcmds b n ls -- 태그 대량생성 방지
                 | all isSpace l                    = printf ":!echo '#CMD%05d'" n : addcmds b (n+1) ls
                 | otherwise                        = l : addcmds b n ls

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
 -- | has
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
                    1 -> atag : addOutTags ls
                    n -> concat ["</a></div>" , atag] : addOutTags ls
  | hasRAW l   = decode (parseRAW l) : addOutTags ls
  | otherwise  = l : addOutTags ls
      where  atag = concat 
                        ["<div id='CMD", 
                        (printf "%05d" (checkCMD l :: Int)) ,
                        "' class='output'><a href='test-in-raw.html",
                        "#", "CMD", (printf "%05d" (checkCMD l :: Int)),
                        "' class='output' target='in'>"
                        ]
    
    
hasRAW :: String -> Bool
hasRAW l = "</FONT></B>GHCIMONRAW <B>" `isPrefixOf` l

parseRAW l = decode $ takeWhile (/= '<') .  filterStr ">" $ filterStr "<FONT " l  --"(&quot;) 에 대한 처리?

hasCMD :: String -> Bool
hasCMD l = "#CMD" `isInfixOf` l 

checkCMD l = read $ filter isNumber (filterStr "#CMD" l) :: Int 

filterStr' :: String -> String -> String -> String
filterStr' [] l _ =  l
filterStr' s  [] _ =  [] 
filterStr' s@(x:xs)  (y:ys) s2 = if x==y then filterStr' xs ys s2 else filterStr' s2 ys s2
filterStr s s2 = filterStr' s s2 s