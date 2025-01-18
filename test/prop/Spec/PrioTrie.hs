{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use &&" #-}
module Spec.PrioTrie (spec) where

import qualified Test.Tasty                         as Tasty
import qualified Test.Tasty.QuickCheck as TQC

import qualified Test.QuickCheck
import qualified Data.ByteString.Char8
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS
import qualified Data.PrioTrie
import Test.QuickCheck (arbitrary, forAll)
import Data.Foldable (foldl')
import Data.Maybe (isNothing, fromMaybe)

spec :: Tasty.TestTree
spec =
    Tasty.testGroup "Data.PrioTrie"
      [ Tasty.testGroup "Keys returned by 'prefixLookup prioTrie bs' have 'bs' as prefix"
          [ setMaxRatio 100 $
              -- Property 1: the items returned by passing an arbitrary 'BS.ByteString' @bs@ to 'prefixLookup' on a 'PrioTrie' constructed using @fromListDeriveKey fModify deriveKey@ will return items such that @bs@ is a prefix of the 'BS.ByteString' you get from applying @deriveKey@ to the returned items.
              TQC.testProperty "Arbitrary prefix" $ forAll arbitraryPrioItems $ \prioItems -> do
                let deriveKey = Data.ByteString.Char8.pack . show
                    genPrefix = Data.ByteString.Char8.pack <$> arbitrary
                forAll genPrefix $ \prefix -> do
                  fromMaybe Test.QuickCheck.discard $
                    properties deriveKey prioItems prefix
          , scaleNumTests (*100) $
            setMaxRatio 1 $ do -- no discards allowed because the generated prefix is known to exist
              TQC.testProperty "Chosen prefix" $ forAll arbitraryPrioItems $ \prioItems -> do
                let deriveKey = Data.ByteString.Char8.pack . show
                    genPrefix =
                      let arbitraryPrefix bs = (`BS.take` bs) <$> Test.QuickCheck.choose (0, BS.length bs)
                      in Test.QuickCheck.oneof $ map (arbitraryPrefix . deriveKey . snd) $ NE.toList prioItems
                forAll genPrefix $ \prefix ->
                  fromMaybe Test.QuickCheck.discard $
                    properties deriveKey prioItems prefix
          ]
      , scaleNumTests (*100) $
        Tasty.testGroup "'prefixLookup' returns 'Nothing' for a string that is not the prefix of any key"
          [ TQC.testProperty "Arbitrary prefix" $ forAll arbitraryPrioItems $ \prioItems -> do
              let deriveKey = Data.ByteString.Char8.pack . show
                  keys = NE.map (deriveKey . snd) prioItems
              let genPrefix = Test.QuickCheck.suchThat (Data.ByteString.Char8.pack <$> arbitrary) $ \bs ->
                    not $ any (BS.isPrefixOf bs) keys
              forAll genPrefix $ \prefix -> do
                isNothing $ properties deriveKey prioItems prefix
          ]
      ]
    where
        setMaxRatio maxRatio =
          Tasty.localOption (TQC.QuickCheckMaxRatio maxRatio)

        scaleNumTests f =
          Tasty.adjustOption (\(TQC.QuickCheckTests v) -> TQC.QuickCheckTests $ f v)

        arbitraryPrioItems = do
          prioAndAList :: Test.QuickCheck.NonEmptyList (Int, Test.QuickCheck.Large Int) <- arbitrary
          pure $ NE.fromList $ Test.QuickCheck.getNonEmpty (fmap Test.QuickCheck.getLarge <$> prioAndAList)

properties
  :: (Ord triePrio, Show triePrio, Show trieItem, Eq trieItem)
  => (trieItem -> BS.ByteString)
  -- ^ Derive a key from a trie item
  -> NE.NonEmpty (triePrio, trieItem)
  -- ^ Build a 'Data.PrioTrie.PrioTrie' from this @(priority, item)@ list
  -> BS.ByteString
  -- ^ Prefix to test
  -> Maybe Bool
  -- ^ 'Just': a 'Bool' indicating whether the test was a success or not.
  --   'Nothing': the test was discarded
properties deriveKey prioItems prefix = do
  let mResult = Data.PrioTrie.prefixLookup prioTrie prefix
  assertions <$> mResult
  where
    prioTrie = Data.PrioTrie.fromListDeriveKey id deriveKey prioItems

    -- TODO: factor out into individual properties?
    assertions results =
      and
        [ all
            (\result@(_, item) ->
                prefix `BS.isPrefixOf` deriveKey item
                && result `elem` prioItems
            )
            results
        , assertDecreasing $ NE.map fst results
        , all (`elem` results) validResults -- no valid results left out
        ]

    -- all items in 'prioItems' whose key have the given prefix.
    validResults =
        map (\(triePrio, (trieItem, _)) -> (triePrio, trieItem))
      $ NE.filter (\(_, (_, key)) -> prefix `BS.isPrefixOf` key)
      $ fmap (\trieItem -> (trieItem, deriveKey trieItem)) <$> prioItems

    assertDecreasing :: (Ord a, Show a) => NE.NonEmpty a -> Bool
    assertDecreasing lst@(head' NE.:| tail') =
      let throwError (prevNum, nextNum) =
            error $ unwords
              [ "Non-decreasing list."
              , show nextNum
              , "is greater than"
              , show prevNum
              , "in"
              , show (NE.toList lst)
              ]

          folder ePrevNum nextNum =
            case ePrevNum of
              Right prevNum ->
                if nextNum <= prevNum
                  then Right nextNum
                  else Left (nextNum, prevNum)
              left -> left
      in either
          throwError
          (const True)
          (foldl' folder (Right head') tail')
