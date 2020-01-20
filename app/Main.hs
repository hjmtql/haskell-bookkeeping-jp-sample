module Main where

import           ClassyPrelude

import qualified Csv
import qualified Expenses
import qualified Sales
import           Business.Bookkeeping

main :: IO ()
main = do
  -- 仕訳帳
  writeFileUtf8 "dist/transactions-2016.csv" $ Csv.fromTransactions trans2016

  -- 総勘定元帳
  writeFileUtf8 "dist/chidaiyachin-2016.csv"
    $ Csv.fromTransactionsToLedger "地代家賃" trans2016

  writeFileUtf8 "dist/suidokonetsuhi-2016.csv"
    $ Csv.fromTransactionsToLedger "水道光熱費" trans2016

  writeFileUtf8 "dist/ryohikotsuhi-2016.csv"
    $ Csv.fromTransactionsToLedger "旅費交通費" trans2016

  writeFileUtf8 "dist/shomohinhi-2016.csv"
    $ Csv.fromTransactionsToLedger "消耗品費" trans2016

  writeFileUtf8 "dist/urikakekin-2016.csv"
    $ Csv.fromTransactionsToLedger "売掛金" trans2016

  writeFileUtf8 "dist/uriagedaka-2016.csv"
    $ Csv.fromTransactionsToLedger "売上高" trans2016

  writeFileUtf8 "dist/jigyonushikashi-2016.csv"
    $ Csv.fromTransactionsToLedger "事業主貸" trans2016

  writeFileUtf8 "dist/jigyonushikari-2016.csv"
    $ Csv.fromTransactionsToLedger "事業主借" trans2016

trans2016 :: Transactions
trans2016 = do
  Expenses.transactions 2016
  Sales.transactions 2016
