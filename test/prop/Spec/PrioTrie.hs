{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}
module Spec.PrioTrie (spec, setup) where

import Paths_function_graph (getDataFileName)
import qualified Control.Monad.ST as ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.List.NonEmpty as NE
import qualified Data.PrioTrie
import qualified FunGraph.Build as FunGraph
import qualified FunGraph.Types as FunGraph
import qualified Server.Pages.Typeahead
import qualified Test.QuickCheck
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as TQC
import Data.Foldable (foldl')
import Data.Maybe (isNothing, fromMaybe)
import Test.QuickCheck (arbitrary, forAll)
import qualified Control.Exception as Ex
import qualified Control.DeepSeq

setup :: IO (Data.PrioTrie.PrioTrie Word FunGraph.FullyQualifiedType)
setup = do
  putStrLn "Constructing priority trie..."
  graphData <- getDataFileName "data/all3.json" >>= FunGraph.fileReadDeclarationMap >>= either fail pure
  graph <- ST.stToIO $ FunGraph.buildGraphMut FunGraph.defaultBuildConfig graphData
  prioTrie <- Server.Pages.Typeahead.mkPrioTrie Nothing graph >>= maybe (fail "empty graph data file") pure
  prioTrie' <- Ex.evaluate $ Control.DeepSeq.force prioTrie
  putStrLn "Done!"
  pure prioTrie'

arbitraryPrefix
  :: (Ord triePrio, Show triePrio, Show trieItem, Eq trieItem)
  => (trieItem -> Data.ByteString.Char8.ByteString)
  -> Data.PrioTrie.PrioTrie triePrio trieItem
  -> TQC.Property
arbitraryPrefix deriveKey prioTrie = do
  let genPrefix = Data.ByteString.Char8.pack <$> arbitrary
  forAll genPrefix $ \prefix -> do
    fromMaybe Test.QuickCheck.discard $
      properties deriveKey prioTrie prefix

chosenPrefix
  :: (Ord triePrio, Show triePrio, Show trieItem, Eq trieItem)
  => (trieItem -> Data.ByteString.Char8.ByteString)
  -> Data.PrioTrie.PrioTrie triePrio trieItem
  -> TQC.Property
chosenPrefix deriveKey prioTrie = do
  let prioItems = snd <$> Data.PrioTrie.toListDeriveKey deriveKey prioTrie
      genPrefix =
        let mkArbitraryPrefix bs = (`BS.take` bs) <$> Test.QuickCheck.choose (0, BS.length bs)
        in Test.QuickCheck.oneof $ map (mkArbitraryPrefix . deriveKey . snd) $ NE.toList prioItems
  forAll genPrefix $ \prefix ->
    fromMaybe Test.QuickCheck.discard $
      properties deriveKey prioTrie prefix

notAPrefix
  :: (Ord triePrio, Show triePrio, Show trieItem, Eq trieItem)
  => (trieItem -> Data.ByteString.Char8.ByteString)
  -> Data.PrioTrie.PrioTrie triePrio trieItem
  -> TQC.Property
notAPrefix deriveKey prioTrie = do
  let prioItems = snd <$> Data.PrioTrie.toListDeriveKey deriveKey prioTrie
      keys = NE.map (deriveKey . snd) prioItems
      genPrefix = Test.QuickCheck.suchThat (Data.ByteString.Char8.pack <$> arbitrary) $ \bs ->
        not $ any (BS.isPrefixOf bs) keys
  forAll genPrefix $ \prefix -> do
    isNothing $ properties deriveKey prioTrie prefix

spec
  :: Data.PrioTrie.PrioTrie Word FunGraph.FullyQualifiedType
  -> Tasty.TestTree
spec productionPrioTrie =
    Tasty.testGroup "Data.PrioTrie"
      [ Tasty.testGroup "Keys returned by 'prefixLookup prioTrie bs' have 'bs' as prefix"
        [ Tasty.testGroup "Arbitrary prefix"
          [ setMaxRatio 100 $
              TQC.testProperty "Arbitrary PrioTrie" $ forAll arbitraryPrioTrie (arbitraryPrefix arbitraryPrioItemsDeriveKey)
          , setMaxRatio 100 $ -- TODO: only one test without a limit; add tests with: limit=1000; limit=100; limit=10
              TQC.testProperty "Production PrioTrie" $ arbitraryPrefix productionPrioItemsDeriveKey productionPrioTrie
          ]
        , Tasty.testGroup "Chosen prefix"
            [ scaleNumTests (*100) $
              setMaxRatio 1 $ do -- no discards allowed because the generated prefix is known to exist
                TQC.testProperty "Arbitrary PrioTrie" $ forAll arbitraryPrioTrie (chosenPrefix arbitraryPrioItemsDeriveKey)
            , setMaxRatio 1 $
                TQC.testProperty "Production PrioTrie" $ chosenPrefix productionPrioItemsDeriveKey productionPrioTrie
            ]
        ]
      , scaleNumTests (*100) $
        Tasty.testGroup "'prefixLookup' returns 'Nothing' for a string that is not the prefix of any key"
          [ TQC.testProperty "Arbitrary prefix" $ forAll arbitraryPrioTrie (notAPrefix arbitraryPrioItemsDeriveKey)
          ]
      ]
    where
        setMaxRatio maxRatio =
          Tasty.localOption (TQC.QuickCheckMaxRatio maxRatio)

        scaleNumTests f =
          Tasty.adjustOption (\(TQC.QuickCheckTests v) -> TQC.QuickCheckTests $ f v)

        arbitraryPrioTrie = do
          prioAndAList :: Test.QuickCheck.NonEmptyList (Int, Test.QuickCheck.Large Int) <- arbitrary
          let prioItems = NE.fromList $ Test.QuickCheck.getNonEmpty (fmap Test.QuickCheck.getLarge <$> prioAndAList)
          pure $ Data.PrioTrie.fromListDeriveKey id arbitraryPrioItemsDeriveKey prioItems

        arbitraryPrioItemsDeriveKey = Data.ByteString.Char8.pack . show

        productionPrioItemsDeriveKey = Server.Pages.Typeahead.deriveKey

properties
  :: (Ord triePrio, Show triePrio, Show trieItem, Eq trieItem)
  => (trieItem -> BS.ByteString)
  -- ^ Derive a key from a trie item
  -> Data.PrioTrie.PrioTrie triePrio trieItem
  -> BS.ByteString
  -- ^ Prefix to test
  -> Maybe Bool
  -- ^ 'Just': a 'Bool' indicating whether the test was a success or not.
  --   'Nothing': the test was discarded
properties deriveKey prioTrie prefix = do
  let mResult = Data.PrioTrie.prefixLookup prioTrie prefix
  assertions <$> mResult
  where
    prioItems = snd <$> Data.PrioTrie.toListDeriveKey deriveKey prioTrie

    -- TODO: factor out into individual properties.
    --       needed in order to e.g. limit the number of results asserted on since "no valid results left out" need all results.
    assertions results =
      and
        [ all -- for all results:
            (\result@(_, item) ->
                prefix `BS.isPrefixOf` deriveKey item -- 'deriveKey' applied to result has the given prefix
                && result `elem` prioItems -- result and priority actually exists in PrioTrie
            )
            results
        , let priorities = NE.map fst results in assertDecreasing priorities -- result priorities are decreasing
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
