-- vim: ts=2: sw=2: expandtab: ai:

module AddEcho where

import Lib

import System.IO

main = do
          putStr . inputFilter =<< hGetContents stdin
