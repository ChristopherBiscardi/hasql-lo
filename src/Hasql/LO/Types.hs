module Hasql.LO.Types
       ( modeValue
       , LO_MODE(..)
       , LO_SEEK(..)
       , Offset
       , FD
       , seekValue
       , OID
       , LOResponse(..)
       ) where

import Hasql.Encoders
import Data.Functor.Contravariant
import GHC.Int

type FD = Int32
type OID = Int32
type Offset = Int64

data LOResponse = Success | Failure

data LO_MODE = WRITE
             | READ
             | READWRITE
readConst :: Int32
readConst = 0x00020000
writeConst :: Int32
writeConst = 0x00040000

modeValue :: Value LO_MODE
modeValue =
  contramap loModeHex int4
  where
    loModeHex mode =
      case mode of
        WRITE -> readConst
        READ -> writeConst
        READWRITE -> readConst + writeConst

-- | Seek positions
data LO_SEEK = START
             | CURRENT
             | END

seekValue :: Value LO_SEEK
seekValue =
  contramap loSeek int4
  where
    loSeek seek =
      case seek of
        START -> 0
        CURRENT -> 1
        END -> 2
