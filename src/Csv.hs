module Csv
  ( fromTransactions
  , fromTransactionsToLedger
  )
where

import           Business.Bookkeeping
import           ClassyPrelude

fromTransactionsToLedger :: Text -> Transactions -> Text
fromTransactionsToLedger name =
  unlines
    . (transactionLedgerHeader :)
    . fmap transactionLedgerBody
    . mkLedgers name
    . numberedTransactions

fromTransactions :: Transactions -> Text
fromTransactions =
  unlines . (transactionHeader :) . fmap transactionBody . numberedTransactions

numberedTransactions :: Transactions -> [NumberedJournal]
numberedTransactions = zip [1 ..] . runTransactions

transactionBody :: NumberedJournal -> Text
transactionBody (n, Journal {..}) = intercalate
  ","
  [ tshow n
  , tshow $ formatTime defaultTimeLocale "%Y/%m/%d" tDay
  , formatText . unCategoryName . cName . unDebitCategory $ tDebit
  , formatText . unCategoryName . cName . unCreditCategory $ tCredit
  , formatText . unDescription $ tDescription
  , formatText . unSubDescription $ tSubDescription
  , tshow $ unAmount tAmount
  ]

transactionLedgerBody :: LedgerRecord -> Text
transactionLedgerBody LedgerRecord {..} = intercalate
  ","
  [ tshow jNo
  , tshow $ formatTime defaultTimeLocale "%Y/%m/%d" jDay
  , formatText . unCategoryName . cName $ jOpposite
  , formatText . unDescription $ jDescription
  , formatText . unSubDescription $ jSubDescription
  , tshow $ unAmount jDebitAmount
  , tshow $ unAmount jCreditAmount
  ]

formatText :: Text -> Text
formatText = (<> "\"") . ("\"" <>)

transactionHeader :: Text
transactionHeader = intercalate
  ","
  [ formatText "取引No"
  , formatText "取引日"
  , formatText "借方勘定科目"
  , formatText "貸方勘定科目"
  , formatText "摘要"
  , formatText "細目"
  , formatText "貸借金額"
  ]

transactionLedgerHeader :: Text
transactionLedgerHeader = intercalate
  ","
  [ formatText "取引No"
  , formatText "取引日"
  , formatText "相手方勘定科目"
  , formatText "摘要"
  , formatText "細目"
  , formatText "借方金額"
  , formatText "貸方金額"
  ]

data LedgerRecord = LedgerRecord
  {  jNo :: Int
  ,  jDay :: Day
  ,  jDescription :: Description
  ,  jSubDescription :: SubDescription
  ,  jOpposite :: Category
  ,  jDebitAmount :: Amount
  ,  jCreditAmount :: Amount
  }

type NumberedJournal = (Int, Journal)

debitLedgerRecord :: NumberedJournal -> LedgerRecord
debitLedgerRecord (n, Journal {..}) = LedgerRecord
  { jNo             = n
  , jDay            = tDay
  , jDescription    = tDescription
  , jSubDescription = tSubDescription
  , jOpposite       = unCreditCategory tCredit
  , jDebitAmount    = tAmount
  , jCreditAmount   = 0
  }

creditLedgerRecord :: NumberedJournal -> LedgerRecord
creditLedgerRecord (n, Journal {..}) = LedgerRecord
  { jNo             = n
  , jDay            = tDay
  , jDescription    = tDescription
  , jSubDescription = tSubDescription
  , jOpposite       = unDebitCategory tDebit
  , jDebitAmount    = 0
  , jCreditAmount   = tAmount
  }

mkLedgers :: Text -> [NumberedJournal] -> [LedgerRecord]
mkLedgers name = mapMaybe (toLedgerRecord name)

toLedgerRecord :: Text -> NumberedJournal -> Maybe LedgerRecord
toLedgerRecord name nj@(_, j) = case j of
  Journal { tDebit = DebitCategory (Category dName _), tCredit = CreditCategory (Category cName _) }
    | unCategoryName cName == name
    -> Just $ creditLedgerRecord nj
    | unCategoryName dName == name
    -> Just $ debitLedgerRecord nj
    | otherwise
    -> Nothing

