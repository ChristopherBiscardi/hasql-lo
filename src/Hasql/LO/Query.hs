{-# LANGUAGE OverloadedStrings #-}
module Hasql.LO.Query where

import           Contravariant.Extras.Contrazip (contrazip3)
import           Data.ByteString                (ByteString)
import           Data.Functor.Contravariant
import           Data.Monoid
import           GHC.Int
import qualified Hasql.Decoders                 as HD
import qualified Hasql.Encoders                 as HE
import qualified Hasql.Query                    as H

import           Hasql.LO.Types                 (FD, LO_MODE, LO_SEEK, Offset,
                                                 modeValue, seekValue, OID)

loCreat :: H.Query () OID
loCreat = H.statement "SELECT lo_creat(-1)"
  HE.unit (HD.singleRow $ HD.value HD.int4) True

loOpen :: H.Query (OID, LO_MODE) FD
loOpen = H.statement "SELECT lo_open($1, $2) AS fd"
  encoders (HD.singleRow $ HD.value HD.int4) True
    where
      encoders = contramap fst (HE.value HE.int4) <>
                 contramap snd (HE.value modeValue)

-- | TODO
-- | return: 0 is success -1 is error
loClose :: H.Query (FD) Int32
loClose = H.statement "SELECT lo_close($1)"
  encoders (HD.singleRow $ HD.value HD.int4) True
    where
      encoders = HE.value HE.int4

loWrite :: H.Query (FD, ByteString) ()
loWrite = H.statement "SELECT lowrite($1, $2)"
  encoders HD.unit True
    where
      encoders = contramap fst (HE.value HE.int4) <>
                 contramap snd (HE.value HE.bytea)


-- | https://www.postgresql.org/docs/9.5/static/lo-funcs.html#LO-FUNCS
loFromByteA :: H.Query (OID, ByteString) OID
loFromByteA = H.statement "SELECT lo_from_bytea($1, $2)" encoder decoder True
  where
    encoder = contramap fst (HE.value HE.int4) <>
              contramap snd (HE.value HE.bytea)
    decoder = HD.singleRow $ HD.value HD.int4

-- | OID, Offset, data to append
loPut :: H.Query (OID, Offset, ByteString) ()
loPut = H.statement "SELECT lo_put($1, $2, $3)"
  encoder decoder True
  where
    encoder = contrazip3 (HE.value HE.int4)
                         (HE.value HE.int8)
                         (HE.value HE.bytea)
    decoder = HD.unit

-- | OID, Offset, data to append
loLSeek :: H.Query (FD, Offset, LO_SEEK) ()
loLSeek = H.statement "PERFORM lo_lseek($1, $2, $3)"
  encoder decoder True
  where
    encoder = contrazip3 (HE.value HE.int4)
                         (HE.value HE.int8)
                         (HE.value seekValue)
    decoder = HD.unit

-- | OID, Offset, data to append
loTell :: H.Query (FD) Int32
loTell = H.statement "SELECT lo_tell($1)"
  encoder decoder True
  where
    encoder = HE.value HE.int4
    decoder = HD.singleRow (HD.value HD.int4)
