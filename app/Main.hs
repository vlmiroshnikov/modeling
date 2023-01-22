module Main (main) where

import Demo
import PrettyPrint (prettyProject)

main :: IO ()
main = putStrLn (prettyProject someProject)
