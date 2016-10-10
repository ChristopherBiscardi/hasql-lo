module Hasql.LO
       ( create
       , createFromByteString
       , open
       , put
       , write
       ) where

import           Data.ByteString   (ByteString)
import           GHC.Int
import qualified Hasql.Decoders    as HD
import qualified Hasql.Encoders    as HE
import qualified Hasql.Query       as H
import qualified Hasql.Session     as H
import qualified Hasql.Transaction as HT

import           Hasql.LO.Query
import           Hasql.LO.Types    (FD, LO_MODE, LO_SEEK, OID, Offset,
                                    LOResponse (..))

-- | Create a new Large Object
create :: HT.Transaction OID
create = HT.query () loCreat

-- | Open a Large Object for writing or reading
open :: OID -> LO_MODE -> HT.Transaction FD
open oid lo_mode = HT.query (oid, lo_mode) loOpen

close :: FD -> HT.Transaction LOResponse
close fd = do
  result <- HT.query (fd) loClose
  case result of
    0 -> return Success
    -- | technically -1 is failure
    otherwise -> return Failure

-- | Write to an open Large Object
write :: FD -> ByteString -> HT.Transaction ()
write oid bs = HT.query (oid, bs) loWrite

-- |
-- Pass `Nothing` if you want PG to create the OID
createFromByteString :: Maybe OID
                     -> ByteString
                     -> HT.Transaction OID
createFromByteString oid bs = case oid of
  -- | OID 0 tells Postgres to create a new OID
  Nothing -> HT.query (0, bs) loFromByteA
  Just i -> HT.query (i, bs) loFromByteA

-- | Append data to a Large Object
put :: OID -> Offset -> ByteString -> HT.Transaction ()
put oid offset bs = HT.query (oid, offset, bs) loPut

-- | Takes an open Large Object, and seeks to the Offset from LO_SEEK
-- location
seek :: FD -> Offset -> LO_SEEK -> HT.Transaction ()
seek fd offset seek = HT.query (fd, offset, seek) loLSeek

-- | Obtain the current read or write location of a large object
-- descriptor.
--
-- To obtain the size of a Large Object, first `seek` to the end
tell :: FD -> HT.Transaction Int32
tell fd = HT.query (fd) loTell
