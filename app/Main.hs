{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Demo
import PrettyPrint (prettyProject, prettyReport)
import Reporting

main :: IO ()
main = do 
    pr      <- calculateProjectReport someProject
    putStrLn (prettyProject prettyReport prettyReport pr)
