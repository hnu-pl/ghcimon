-- vim: ts=2: sw=2: expandtab: ai:

module TagOutput where

import Lib

import System.IO

main = do
          putStr . tagOutputHTML =<< hGetContents stdin
