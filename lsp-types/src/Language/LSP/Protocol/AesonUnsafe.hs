{-# LANGUAGE MagicHash #-}
module Language.LSP.Protocol.AesonUnsafe
    ( valueConName
    , unknownFieldFail
    , unsafePackLenLiteral
    , parseTypeMismatch'
    , fromPairs
    , pair
    , lookupFieldOmit
    ) where

import Data.Aeson.Types
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Encoding.Internal as EI
import qualified Data.DList as DList
import Text.Printf

import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short.Internal (createFromPtr)
import GHC.Exts (Addr#, Ptr (Ptr))
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | The name of the outermost 'Value' constructor.
valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"

lookupFieldOmit :: Maybe a -> (Value -> Parser a) -> String -> String -> Object -> Key -> Parser a
lookupFieldOmit maybeDefault pj tName rec obj key =
    case KM.lookup key obj of
      Nothing ->
        case maybeDefault of
          Nothing -> unknownFieldFail tName rec (Key.toString key)
          Just x -> pure x
      Just v  -> pj v <?> Key key

unknownFieldFail :: String -> String -> String -> Parser fail
unknownFieldFail tName rec key =
    fail $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

parseTypeMismatch' :: String -> String -> String -> String -> Parser fail
parseTypeMismatch' conName tName expected actual =
    fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName tName expected actual

-- | Wrap a list of pairs as an object.
class Monoid pairs => FromPairs enc pairs | enc -> pairs where
  fromPairs :: pairs -> enc

instance (a ~ Value) => FromPairs (EI.Encoding' a) Series where
  fromPairs = EI.pairs
  {-# INLINE fromPairs #-}

instance FromPairs Value (DList.DList Pair) where
  fromPairs = object . DList.toList
  {-# INLINE fromPairs #-}

class Monoid kv => KeyValuePair v kv where
    pair :: Key -> v -> kv

instance (v ~ Value) => KeyValuePair v (DList.DList Pair) where
    pair k v = DList.singleton (k .= v)
    {-# INLINE pair #-}

instance (e ~ Encoding) => KeyValuePair e Series where
    pair = EI.pair
    {-# INLINE pair #-}

unsafePackLenLiteral :: Int -> Addr# -> ShortByteString
unsafePackLenLiteral len addr# =
    unsafeDupablePerformIO $ createFromPtr (Ptr addr#) len
