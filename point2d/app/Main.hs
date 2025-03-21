module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = getArgs >>= print
