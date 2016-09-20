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

import           Hasql.LO.Types                 (LO_MODE, modeValue)

loCreat :: H.Query () Int32
loCreat = H.statement "SELECT lo_creat(-1)"
  HE.unit (HD.singleRow $ HD.value HD.int4) True

loOpen :: H.Query (Int32, LO_MODE) Int32
loOpen = H.statement "SELECT lo_open($1, $2) AS fd"
  encoders (HD.singleRow $ HD.value HD.int4) True
    where
      encoders = contramap fst (HE.value HE.int4) <>
                 contramap snd (HE.value modeValue)

loWrite :: H.Query (Int32, ByteString) ()
loWrite = H.statement "SELECT lowrite($1, $2)"
  encoders HD.unit True
    where
      encoders = contramap fst (HE.value HE.int4) <>
                 contramap snd (HE.value HE.bytea)


-- | https://www.postgresql.org/docs/9.5/static/lo-funcs.html#LO-FUNCS
loFromByteA :: H.Query (Int32, ByteString) Int32
loFromByteA = H.statement "SELECT lo_from_bytea($1, $2)" encoder decoder True
  where
    encoder = contramap fst (HE.value HE.int4) <>
              contramap snd (HE.value HE.bytea)
    decoder = HD.singleRow $ HD.value HD.int4

-- | OID, Offset, data to append
loPut :: H.Query (Int32, Int64, ByteString) ()
loPut = H.statement "SELECT lo_put($1, $2, $3)"
  encoder decoder True
  where
    encoder = contrazip3 (HE.value HE.int4)
                         (HE.value HE.int8)
                         (HE.value HE.bytea)
    decoder = HD.unit
