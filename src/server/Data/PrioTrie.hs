{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.PrioTrie
( PrioTrie(..)
, prefixLookup
, fromList)
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.List.NonEmpty as NE
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic, Generic1)

data PrioTrie prio a
  = PrioTrie_Node
      !(IMap.IntMap (PrioTrie prio a))
      -- ^ Children
      !(NE.NonEmpty (prio, a))
      -- ^ List containing /all/ items that match the prefix reached through the 'IntMap'
      deriving (Eq, Show, Generic, Generic1)

instance (NFData prio, NFData a) => NFData (PrioTrie prio a)

unitTest :: (PrioTrie Integer String, PrioTrie Integer String)
unitTest =
  (fromList id inputList, trie)
  where
    inputList = NE.fromList
      [ ("a", (2, "2"))
      , ("aa", (3, "3"))
      , ("ab", (1, "1"))
      , ("c", (4, "4"))
      ]

    trie =
      PrioTrie_Node
        (IMap.fromList
          [ ( w 'a'
            , PrioTrie_Node
                (IMap.fromList
                  [ ( w 'a'
                    , PrioTrie_Node
                        IMap.empty
                        (NE.fromList [(3, "3")])
                    )
                  , ( w 'b'
                    , PrioTrie_Node
                        IMap.empty
                        (NE.fromList [(1, "1")])
                    )
                  ]
                )
                (NE.fromList [(3, "3"), (2, "2"), (1, "1")])
            )
          , ( w 'c'
            , PrioTrie_Node
                IMap.empty
                (NE.fromList [(4, "4")])
            )
          ]
        )
        (NE.fromList $ sortOn (Down . fst) $ map snd (NE.toList inputList))

    w :: Char -> Int
    w = fromIntegral . BSI.c2w

fromList
  :: forall prio a.
     Ord prio
  => (NE.NonEmpty (prio, a) -> NE.NonEmpty (prio, a))
     -- ^ Modify the sorted list stored at each 'PrioTrie_Node'.
     -- Used to e.g. limit the number of items in the list.
  -> NE.NonEmpty (BS.ByteString, (prio, a))
  -> PrioTrie prio a
fromList modify lst =
  let
      initMap :: Map.Map BS.ByteString (NE.NonEmpty (prio, a)) -- unsorted!
      -- ByteString -> [(144, Data.Internal.ByteString), (37, Data.Internal.Lazy.ByteString)]
      -- String -> [(12344, Data.String.String)]
      -- Tree -> [(13, Data.Tree.Tree), (7, Data.Tree.Special.Tree)]
      initMap =
          Map.fromListWith
          (\(a NE.:| []) neList -> NE.cons a neList)
          (map (fmap NE.singleton) (NE.toList lst))

      mkUnconsMap
        :: Map.Map BS.ByteString (NE.NonEmpty (prio, a))
        -> Map.Map (Word8, BS.ByteString) (NE.NonEmpty (prio, a))
      mkUnconsMap map' =
        Map.mapKeys fromJust $ -- safe because all 'Nothing' keys have been removed
          Map.withoutKeys
            (Map.mapKeys BS.uncons map')
            (Set.singleton Nothing)

      mkPrefixToList
        :: Map.Map (Word8, BS.ByteString) (NE.NonEmpty (prio, a)) -- output of 'mkUnconsMap'
        -> Map.Map Word8 (NE.NonEmpty (BS.ByteString, NE.NonEmpty (prio, a))) -- input to 'fromList' for each Word8-prefix
      mkPrefixToList =
        Map.fromListWith (<>) -- TODO: performance
          . map (\((w8, bs), ne) -> (w8, NE.singleton (bs, ne)))
          . Map.toList

      mkIntMap
        :: Map.Map Word8 (NE.NonEmpty (BS.ByteString, NE.NonEmpty (prio, a))) -- output of 'mkPrefixToList'
        -> IMap.IntMap (PrioTrie prio a) -- first argument to 'PrioTrie_Node'
      mkIntMap =
        IMap.fromList
          . Map.toList
          . Map.map (fromList modify)
          . Map.map (NE.fromList . concatMap (\(bs, ne) -> map (bs,) (NE.toList ne)) . NE.toList)
          . Map.mapKeys fromIntegral

      intMap :: IMap.IntMap (PrioTrie prio a)
      intMap =
        mkIntMap $ mkPrefixToList $ mkUnconsMap initMap

      -- the non-empty input list sorted by priority (descending)
      nonEmpty = modify $
        NE.fromList . sortOn (Down . fst) $ map snd (NE.toList lst)

  in PrioTrie_Node intMap nonEmpty

prefixLookup
  :: PrioTrie prio a
  -> BS.ByteString
  -> Maybe (NE.NonEmpty (prio, a))
prefixLookup initTrie initBs =
  let go (PrioTrie_Node intMap neList) bs =
        case BS.uncons bs of
          Nothing -> -- end of 'bs' reached
            Just neList
          Just (w8, bsRem) -> --
            case IMap.lookup (fromIntegral w8) intMap of
              Just trie' -> go trie' bsRem
              Nothing -> Nothing
  in go initTrie initBs
