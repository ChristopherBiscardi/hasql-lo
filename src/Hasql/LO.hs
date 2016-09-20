module Hasql.LO
       ( create
       , createFromByteString
       , open
       , put
       , write
       ) where

import           Data.ByteString (ByteString)
import           GHC.Int
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Query as H
import qualified Hasql.Session as H
import qualified Hasql.Transaction as HT

import           Hasql.LO.Query
import           Hasql.LO.Types (LO_MODE)

create :: HT.Transaction Int32
create = HT.query () loCreat

open :: Int32 -> LO_MODE -> HT.Transaction Int32
open oid lo_mode = HT.query (oid, lo_mode) loOpen

write :: Int32 -> ByteString -> HT.Transaction ()
write oid bs = HT.query (oid, bs) loWrite

-- | Pass `Nothing` if you want PG to create the OID
createFromByteString :: Maybe Int32
                     -> ByteString
                     -> HT.Transaction Int32
createFromByteString oid bs = case oid of
  Nothing -> HT.query (0, bs) loFromByteA
  Just i -> HT.query (i, bs) loFromByteA

put :: Int32 -> Int64 -> ByteString -> HT.Transaction ()
put oid offset bs = HT.query (oid, offset, bs) loPut
