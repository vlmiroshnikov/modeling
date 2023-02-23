{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import Data.Decimal (roundTo)
import Data.Tree (Tree (Node), drawTree)
import Text.Printf

import Project
import Reporting

asTree :: (g -> String) -> (a -> String) -> Project g a -> Tree String
asTree _ prettyValue (Project name x) = Node (printf "%s %s" name (prettyValue x)) []
asTree prettyGroup prettyValue (ProjectGroup name g projects) =
    Node (printf "%s %s" name (prettyGroup g)) (map (asTree prettyGroup prettyValue) projects)

prettyProject :: (g -> String) -> (a -> String) -> Project g a -> String
prettyProject prettyGroup prettyValue = drawTree . asTree prettyGroup prettyValue

prettyMoney :: Money -> String
prettyMoney (Money d) = sign ++ show (roundTo 2 d)
  where
    sign = if d > 0 then "+" else ""

prettyReport :: Report -> String
prettyReport r =
    printf
        "Budget: %s, Net: %s, Difference: %s"
        (prettyMoney (budgetProfit r))
        (prettyMoney (netProfit r))
        (prettyMoney (difference r))
