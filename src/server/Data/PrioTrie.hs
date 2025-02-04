{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- TODO: document
module Data.PrioTrie
( PrioTrie
, prefixLookup
, fromList
, fromListDeriveKey, toListDeriveKey
)
where

import qualified Data.List.NonEmpty as NE
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic, Generic1)
import qualified Data.Char
import Data.Bifunctor (first)
import qualified Data.Text as T

data PrioTrie prio a
  = PrioTrie_Node
      !(CharMap (PrioTrie prio a))
      -- ^ Children
      !(NE.NonEmpty (prio, a))
      -- ^ List containing /all/ items that match the prefix reached through the 'IntMap'.
      --
      --   In other words: no need to traverse the entire tree to get all items for a given prefix.
      deriving (Eq, Show, Generic, Generic1)

instance (NFData prio, NFData a) => NFData (PrioTrie prio a)

-- | Construct a 'PrioTrie' from a list of items and their priorities, deriving each 'T.Text' key from the item
fromListDeriveKey
  :: forall prio a.
     Ord prio
  => (NE.NonEmpty (prio, a) -> NE.NonEmpty (prio, a))
     -- ^ Modify the sorted list stored at each 'PrioTrie_Node'.
     -- Used to e.g. limit the number of items in the list.
  -> (a -> T.Text)
     -- ^ Derive a 'T.Text' from the item (@a@) stored in the 'PrioTrie'
  -> NE.NonEmpty (prio, a)
  -> PrioTrie prio a
fromListDeriveKey modify deriveKey lst =
  fromList modify $ NE.map (\prioAndA -> (deriveKey $ snd prioAndA, prioAndA)) lst

-- | Construct a 'PrioTrie' from a list of items and their priorities with the given 'T.Text' as key.
fromList
  :: forall prio a.
     Ord prio
  => (NE.NonEmpty (prio, a) -> NE.NonEmpty (prio, a))
     -- ^ Modify the sorted list stored at each 'PrioTrie_Node'.
     -- Used to e.g. limit the number of items in the list.
  -> NE.NonEmpty (T.Text, (prio, a))
  -> PrioTrie prio a
fromList modify lst =
  let
      initMap :: Map.Map T.Text (NE.NonEmpty (prio, a)) -- unsorted!
      -- ByteString -> [(144, Data.Internal.ByteString), (37, Data.Internal.Lazy.ByteString)]
      -- String -> [(12344, Data.String.String)]
      -- Tree -> [(13, Data.Tree.Tree), (7, Data.Tree.Special.Tree)]
      initMap =
          Map.fromListWith
          (\(a NE.:| []) neList -> NE.cons a neList)
          (map (fmap NE.singleton) (NE.toList lst))

      mkUnconsMap
        :: Map.Map T.Text (NE.NonEmpty (prio, a))
        -> Map.Map (Char, T.Text) (NE.NonEmpty (prio, a))
      mkUnconsMap map' =
        Map.mapKeys fromJust $ -- safe because all 'Nothing' keys have been removed
          Map.withoutKeys
            (Map.mapKeys T.uncons map')
            (Set.singleton Nothing)

      mkPrefixToList
        :: Map.Map (Char, T.Text) (NE.NonEmpty (prio, a)) -- output of 'mkUnconsMap'
        -> Map.Map Char (NE.NonEmpty (T.Text, NE.NonEmpty (prio, a))) -- input to 'fromList' for each Char-prefix
      mkPrefixToList =
        Map.fromListWith (<>) -- TODO: performance
          . map (\((char, txt), ne) -> (char, NE.singleton (txt, ne)))
          . Map.toList

      mkCharMap
        :: Map.Map Char (NE.NonEmpty (T.Text, NE.NonEmpty (prio, a))) -- output of 'mkPrefixToList'
        -> CharMap (PrioTrie prio a) -- first argument to 'PrioTrie_Node'
      mkCharMap =
        fromListCharMap
          . Map.toList
          . Map.map (fromList modify)
          . Map.map (NE.fromList . concatMap (\(txt, ne) -> map (txt,) (NE.toList ne)) . NE.toList)

      charMap :: CharMap (PrioTrie prio a)
      charMap =
        mkCharMap $ mkPrefixToList $ mkUnconsMap initMap

      -- the non-empty input list sorted by priority (descending)
      nonEmpty = modify $
        NE.fromList . sortOn (Down . fst) $ map snd (NE.toList lst)

  in PrioTrie_Node charMap nonEmpty

-- | Convert a 'PrioTrie' to a list of keys and items (along with priority)
--   given the @deriveKey@ function used to construct the 'PrioTrie' using 'fromListDeriveKey'
toListDeriveKey
  :: (a -> T.Text)
     -- ^ Derive a 'T.Text' from the item (@a@) stored in the 'PrioTrie'
  -> PrioTrie prio a
  -> NE.NonEmpty (T.Text, (prio, a))
toListDeriveKey deriveKey (PrioTrie_Node _ neList) =
  NE.map (\prioAndA -> (deriveKey $ snd prioAndA, prioAndA)) neList

prefixLookup
  :: PrioTrie prio a
  -> T.Text
  -> Maybe (NE.NonEmpty (prio, a))
prefixLookup initTrie prefix =
  let go (PrioTrie_Node charMap neList) prefix' =
        case T.uncons prefix' of
          Nothing -> -- end of prefix reached
            Just neList
          Just (char, prefixRem) -> --
            case lookupCharMap char charMap of
              Just trie' -> go trie' prefixRem
              Nothing -> Nothing
  in go initTrie prefix

-- ####################
-- ##    Helpers     ##
-- ####################

-- | A map from a unicode character ('Char') to something
newtype CharMap a = CharMap { unCharMap :: IMap.IntMap a }
  deriving (Eq, Show, Functor, Generic, Generic1, NFData)

lookupCharMap :: Char -> CharMap a -> Maybe a
lookupCharMap c = IMap.lookup (Data.Char.ord c) . unCharMap

fromListCharMap :: [(Char, a)] -> CharMap a
fromListCharMap = CharMap . IMap.fromList . map (first Data.Char.ord)
