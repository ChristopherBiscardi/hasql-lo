{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Lib
    ( someFunc
    ) where

import           Control.Monad.IO.Class
import           GHC.Int
import           Hasql.Connection (settings)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Pool as P
import qualified Hasql.Query as Query
import qualified Hasql.Session as Session
import           Hasql.TH
import qualified Hasql.Transaction as HT

import           Hasql.LO
import           Hasql.LO.Types

readTest :: Query.Query () Int64
readTest = Query.statement $(readFileAsSQL "./src/test.sql") Encoders.unit decoder True
  where
    decoder = Decoders.singleRow (Decoders.value Decoders.int8)

writeToNewLO :: HT.Transaction Int32
writeToNewLO = do
  oid <- createFromByteString Nothing "heya!"
  l <- open oid WRITE
  o <- write l "things"
  return oid

someFunc :: IO ()
someFunc = do
  let pgSettings = settings "localhost" 5432 "postgres" "password" "postgres"
      sql = readTest
  pool <- P.acquire (4, 10, pgSettings)
  result <- P.use pool $ do
    res <- Session.query () sql
    liftIO $ print res
    oid <- HT.run writeToNewLO HT.ReadCommitted HT.Write
    liftIO $ print oid
  print "done"
