-- vim: ts=2: sw=2: expandtab: ai:

module TagInput where

import Lib

import System.IO

main = do
          putStr . tagInputHTML =<< hGetContents stdin
