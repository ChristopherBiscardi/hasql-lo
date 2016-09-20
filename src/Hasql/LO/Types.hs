module Hasql.LO.Types
       ( modeValue
       , LO_MODE(..)
       ) where

import Hasql.Encoders
import Data.Functor.Contravariant
import GHC.Int

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
