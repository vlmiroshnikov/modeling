module Database (getBudget, getTransactions) where

import Project
import System.Random (getStdRandom, randomR)

getBudget :: ProjectId -> IO Budget
getBudget _ = do
    income <- Money <$> getStdRandom (randomR (0, 100))
    expenditure <- Money <$> getStdRandom (randomR (0, 100))
    pure Budget{budgetIncome = income, budgetExpenditure = expenditure}

getTransactions :: ProjectId -> IO [Transaction]
getTransactions _ = do
    sale <- Sale . Money <$> getStdRandom (randomR (0, 4000))
    purchase <- Sale . Money <$> getStdRandom (randomR (0, 4000))
    pure [sale, purchase]
