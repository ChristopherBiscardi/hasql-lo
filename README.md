# Hasql Large Objects

This package contains helpers for using large objects with Hasql, and
thus Postgres.

# Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import           Control.Monad.IO.Class
import           GHC.Int
import           Hasql.Connection (settings)
import qualified Hasql.Pool as P
import qualified Hasql.Transaction as HT

import           Hasql.LO
import           Hasql.LO.Types

-- | Composed Transactions
writeToNewLO :: HT.Transaction Int32
writeToNewLO = do
  oid <- createFromByteString Nothing "heya!"
  l <- open oid WRITE
  o <- write oid "things"
  return oid

someFunc :: IO ()
someFunc = do
  let pgSettings = settings "localhost" 5432 "postgres" "password" "postgres"
  pool <- P.acquire (4, 10, pgSettings)
  result <- P.use pool $ do
    oid <- HT.run writeToNewLO HT.ReadCommitted HT.Write
    liftIO $ print oid
  print "done"
```
