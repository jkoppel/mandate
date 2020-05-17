module Languages.Analysis.ConstProp (
    ConstVal(..)
  , ConstPropState(..)

  , constPropBottom
  , joinConstPropState
  , abstractOutput

  , constValBinop
  , constValUnop
  ) where


import           Data.Map ( Map, (!) )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )

import Data.Interned.ByteString ( InternedByteString )

import CfgGenRuntime
import Term

-------------------------------------------------------------------------

data ConstVal = Top | Bottom | Known Integer
  deriving ( Eq, Ord, Show )

data ConstPropState = ConstPropState {
      constProp_val  :: ConstVal
    , constProp_vars :: Map InternedByteString ConstVal
    }
  deriving (Eq, Ord, Show)

constPropBottom :: Set InternedByteString -> ConstPropState
constPropBottom vars = ConstPropState Bottom $ Map.fromSet (const Bottom) vars

joinConstVal :: ConstVal -> ConstVal -> ConstVal
joinConstVal Top       b         = Top
joinConstVal a         Top       = Top
joinConstVal Bottom    b         = b
joinConstVal a         Bottom    = a
joinConstVal (Known a) (Known b) = if a == b then Known a else Top

joinConstMap :: (Ord k) => Map k ConstVal -> Map k ConstVal -> Map k ConstVal
joinConstMap = Map.unionWith joinConstVal

joinConstPropState :: ConstPropState -> ConstPropState -> ConstPropState
joinConstPropState (ConstPropState val1 vars1)
                   (ConstPropState val2 vars2) = ConstPropState (joinConstVal val1 val2) (joinConstMap vars1 vars2)


abstractOutput :: Map (GraphNode l) ConstPropState -> Term l -> ConstVal
abstractOutput st t = constProp_val $ st ! (nodeForTerm (Map.keys st) t ExitNode)


constValBinop :: (Integer -> Integer -> Integer) -> ConstVal -> ConstVal -> ConstVal
constValBinop op (Known a) (Known b) = Known (a `op` b)
constValBinop _  Bottom    _         = Bottom
constValBinop _  _         Bottom    = Bottom
constValBinop _  Top       _         = Top
constValBinop _  _         Top       = Top


constValUnop :: (Integer -> Integer) -> ConstVal -> ConstVal
constValUnop op Bottom    = Bottom
constValUnop op Top       = Top
constValUnop op (Known n) = Known $ op n