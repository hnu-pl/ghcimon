{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Sample where
import System.Console.CmdArgs
import System.Environment (getArgs)

data Sample = Sample {hello :: String, world :: String, arglist :: [String]}
              deriving (Show, Data, Typeable)

sample = Sample{hello = def, world = def, arglist = def &= args}

main = do print =<< getArgs
          print =<< cmdArgs sample
{-
$ runhaskell Sample.hs aaa --hello=bbb ccc ddd
["aaa","--hello=bbb","ccc","ddd"]
Sample {hello = "bbb", world = "", arglist = ["aaa","ccc","ddd"]}
 -}
