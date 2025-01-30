{-# LANGUAGE ScopedTypeVariables #-}
module Spec.PrioTrie (spec, setup) where

import qualified Control.DeepSeq
import qualified Control.Exception as Ex
import qualified Control.Monad.ST as ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.List.NonEmpty as NE
import qualified Data.PrioTrie
import qualified Data.Set as Set
import qualified FunGraph.Build as FunGraph
import qualified FunGraph.Types as FunGraph
import qualified Server.Pages.Typeahead
import qualified Test.QuickCheck
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as TQC
import Data.Foldable (foldl')
import Data.Maybe (isNothing, fromMaybe)
import Paths_function_graph (getDataFileName)
import Test.QuickCheck (arbitrary, forAll)

setup :: IO (Data.PrioTrie.PrioTrie Word FunGraph.FullyQualifiedType)
setup = do
  putStrLn "Constructing priority trie..."
  graphData <- getDataFileName "data/all3.json" >>= FunGraph.fileReadDeclarationMap >>= either fail pure
  graph <- ST.stToIO $ FunGraph.buildGraphMut FunGraph.defaultBuildConfig graphData
  prioTrie <- Server.Pages.Typeahead.mkPrioTrie Nothing graph >>= maybe (fail "empty graph data file") pure
  prioTrie' <- Ex.evaluate $ Control.DeepSeq.force prioTrie
  putStrLn "Done!"
  pure prioTrie'

-- TODO: production PrioTrie with limit result count
spec
  :: Data.PrioTrie.PrioTrie Word FunGraph.FullyQualifiedType
  -> Tasty.TestTree
spec productionPrioTrie =
    Tasty.testGroup "Data.PrioTrie"
      [ Tasty.testGroup "Positive"
        [ let genPrefix = const $ Data.ByteString.Char8.pack <$> arbitrary
          in Tasty.testGroup "Arbitrary prefix"
          [ Tasty.testGroup "Arbitrary PrioTrie" $
              allProperties
                arbitraryPrioItemsDeriveKey
                (Right arbitraryPrioTrie)
                genPrefix
          , Tasty.testGroup "Production PrioTrie" $
              allProperties
                productionPrioItemsDeriveKey
                (Left productionPrioTrie)
                genPrefix
          ]
        , let genPrefix deriveKey prioTrie =
                let prioItems = snd <$> Data.PrioTrie.toListDeriveKey deriveKey prioTrie
                    mkArbitraryPrefix bs = (`BS.take` bs) <$> Test.QuickCheck.choose (0, BS.length bs)
                in Test.QuickCheck.oneof $ map (mkArbitraryPrefix . deriveKey . snd) $ NE.toList prioItems
          in Tasty.testGroup "Chosen prefix"
          [ Tasty.testGroup "Arbitrary PrioTrie" $
              map (scaleNumTests (*100)) $
              allProperties
                arbitraryPrioItemsDeriveKey
                (Right arbitraryPrioTrie)
                (genPrefix arbitraryPrioItemsDeriveKey)
          , Tasty.testGroup "Production PrioTrie" $
              map (scaleNumTests (*10)) $
              allProperties
                productionPrioItemsDeriveKey
                (Left productionPrioTrie)
                (genPrefix productionPrioItemsDeriveKey)
          ]
        ]
      , Tasty.testGroup "Negative"
        [ Tasty.testGroup "Arbitrary PrioTrie"
            [ scaleNumTests (*100) $
                prefixLookupReturnsNothingForNonPrefix arbitraryPrioItemsDeriveKey (Right arbitraryPrioTrie)
            ]
        , Tasty.testGroup "Production PrioTrie"
            [ scaleNumTests (*100) $
              prefixLookupReturnsNothingForNonPrefix productionPrioItemsDeriveKey (Left productionPrioTrie)
            ]
        ]
      ]
    where
        arbitraryPrioTrie = do
          prioAndAList :: Test.QuickCheck.NonEmptyList (Int, Test.QuickCheck.Large Int) <- arbitrary
          let prioItems = NE.fromList $ Test.QuickCheck.getNonEmpty (fmap Test.QuickCheck.getLarge <$> prioAndAList)
          pure $ Data.PrioTrie.fromListDeriveKey id arbitraryPrioItemsDeriveKey prioItems

        arbitraryPrioItemsDeriveKey = Data.ByteString.Char8.pack . show

        productionPrioItemsDeriveKey = Server.Pages.Typeahead.deriveKey

-- | Either just a value @a@ or a generator ('TQC.Gen') of @a@ values
type TestArgument a =
  Either a (TQC.Gen a)

apply :: Show a => TestArgument a -> (a -> TQC.Property) -> TQC.Property
apply =
  either (\prioTrie -> ($ prioTrie)) forAll

-- ####################
-- ##   Properties   ##
-- ####################

allProperties
  :: (Show triePrio, Show trieItem, Eq triePrio, Eq trieItem, Ord triePrio, Ord trieItem)
  => (trieItem -> Data.ByteString.Char8.ByteString) -- ^ "Derive key"-function
  -> TestArgument (Data.PrioTrie.PrioTrie triePrio trieItem)
  -> (Data.PrioTrie.PrioTrie triePrio trieItem -> TQC.Gen BS.ByteString) -- ^ Prefix generator
  -> [Tasty.TestTree]
allProperties deriveKey ePrioTrie mkPrefixGen =
  [ setMaxRatio 100 $
      deriveKeyAppliedToResultHasTheGivenPrefix deriveKey ePrioTrie mkPrefixGen
  , setMaxRatio 100 $
      resultAndPriorityActuallyExistsInPrioTrie deriveKey ePrioTrie mkPrefixGen
  , setMaxRatio 100 $
      noValidResultsLeftOut deriveKey ePrioTrie mkPrefixGen
  , setMaxRatio 100 $
      resultPrioritiesAreDecreasing deriveKey ePrioTrie mkPrefixGen
  ]

genericProperty
  :: (Show triePrio, Show trieItem, TQC.Testable prop)
  => String -- ^ Property name
  -> TestArgument (Data.PrioTrie.PrioTrie triePrio trieItem)
  -> (Data.PrioTrie.PrioTrie triePrio trieItem -> TQC.Gen BS.ByteString) -- ^ Prefix generator
  -> (Data.PrioTrie.PrioTrie triePrio trieItem -> a) -- ^ Pre-computed value available to the assertion function
  -> (   a
      -> BS.ByteString -- Generated prefix
      -> Maybe (NE.NonEmpty (triePrio, trieItem)) -- Return value of 'Data.PrioTrie.prefixLookup'
      -> prop
     ) -- ^ Assertion function
  -> Tasty.TestTree
genericProperty name ePrioTrie mkPrefixGen assertValue assert =
  TQC.testProperty name $
  apply ePrioTrie $ \prioTrie ->
  forAll (mkPrefixGen prioTrie) $ \prefix ->
  TQC.property $
    assert (assertValue prioTrie) prefix $ Data.PrioTrie.prefixLookup prioTrie prefix

deriveKeyAppliedToResultHasTheGivenPrefix
  :: (Show triePrio, Show trieItem) => (trieItem -> Data.ByteString.Char8.ByteString) -- ^ "Derive key"-function
  -> TestArgument (Data.PrioTrie.PrioTrie triePrio trieItem)
  -> (Data.PrioTrie.PrioTrie triePrio trieItem -> TQC.Gen BS.ByteString) -- ^ Prefix generator
  -> Tasty.TestTree
deriveKeyAppliedToResultHasTheGivenPrefix deriveKey ePrioTrie mkPrefixGen =
  genericProperty
    "'deriveKey' applied to result has the given prefix"
    ePrioTrie
    mkPrefixGen
    (const ())
    prop
  where
    prop () prefix mResults =
      let results = fromMaybe TQC.discard mResults in
      all
      (\(_, item) ->
        prefix `BS.isPrefixOf` deriveKey item
      )
      results

resultAndPriorityActuallyExistsInPrioTrie
  :: (Show triePrio, Show trieItem, Eq triePrio, Eq trieItem, Ord trieItem, Ord triePrio)
  => (trieItem -> Data.ByteString.Char8.ByteString) -- ^ "Derive key"-function
  -> TestArgument (Data.PrioTrie.PrioTrie triePrio trieItem)
  -> (Data.PrioTrie.PrioTrie triePrio trieItem -> TQC.Gen BS.ByteString) -- ^ Prefix generator
  -> Tasty.TestTree
resultAndPriorityActuallyExistsInPrioTrie deriveKey ePrioTrie mkPrefixGen =
  genericProperty
    "result and priority actually exists in PrioTrie"
    ePrioTrie
    mkPrefixGen
    (\prioTrie ->
        let prioItems = snd <$> Data.PrioTrie.toListDeriveKey deriveKey prioTrie
        in Set.fromList $ NE.toList prioItems
    )
    prop
  where
    prop prioItemsSet _ mResults =
      let results = fromMaybe TQC.discard mResults
          resultSet = Set.fromList $ NE.toList results
      in resultSet `Set.isSubsetOf` prioItemsSet

noValidResultsLeftOut
  :: (Show triePrio, Show trieItem, Eq triePrio, Eq trieItem, Ord triePrio, Ord trieItem)
  => (trieItem -> Data.ByteString.Char8.ByteString) -- ^ "Derive key"-function
  -> TestArgument (Data.PrioTrie.PrioTrie triePrio trieItem)
  -> (Data.PrioTrie.PrioTrie triePrio trieItem -> TQC.Gen BS.ByteString) -- ^ Prefix generator
  -> Tasty.TestTree
noValidResultsLeftOut deriveKey ePrioTrie mkPrefixGen =
  genericProperty
    "no valid results left out"
    ePrioTrie
    mkPrefixGen
    (\prioTrie ->
        let prioItems = snd <$> Data.PrioTrie.toListDeriveKey deriveKey prioTrie
            prioItemsWithKey = fmap (\trieItem -> (trieItem, deriveKey trieItem)) <$> prioItems
        in Set.fromList $ NE.toList prioItemsWithKey

    )
    prop
  where
    prop prioItemsWithKeySet prefix mResults =
      let results = fromMaybe TQC.discard mResults
          resultSet = Set.fromList $ NE.toList results
          validResultSet =
              Set.map (\(triePrio, (trieItem, _)) -> (triePrio, trieItem))
            $ Set.filter (\(_, (_, key)) -> prefix `BS.isPrefixOf` key) prioItemsWithKeySet
      in resultSet == validResultSet

resultPrioritiesAreDecreasing
  :: (Show triePrio, Show trieItem, Eq triePrio, Eq trieItem, Ord triePrio)
  => (trieItem -> Data.ByteString.Char8.ByteString) -- ^ "Derive key"-function
  -> TestArgument (Data.PrioTrie.PrioTrie triePrio trieItem)
  -> (Data.PrioTrie.PrioTrie triePrio trieItem -> TQC.Gen BS.ByteString) -- ^ Prefix generator
  -> Tasty.TestTree
resultPrioritiesAreDecreasing _ ePrioTrie mkPrefixGen =
  genericProperty
    "result priorities are decreasing"
    ePrioTrie
    mkPrefixGen
    (const ())
    prop
  where
    prop () _ mResults =
      let results = fromMaybe TQC.discard mResults
          priorities = NE.map fst results
      in assertDecreasing priorities

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

prefixLookupReturnsNothingForNonPrefix
  :: (Show triePrio, Show trieItem)
  => (trieItem -> BS.ByteString)
  -> TestArgument (Data.PrioTrie.PrioTrie triePrio trieItem)
  -> Tasty.TestTree
prefixLookupReturnsNothingForNonPrefix deriveKey ePrioTrie =
  let mkPrefixGen prioTrie =
        let prioItems = snd <$> Data.PrioTrie.toListDeriveKey deriveKey prioTrie
            keys = NE.map (deriveKey . snd) prioItems
        in Test.QuickCheck.suchThat (Data.ByteString.Char8.pack <$> arbitrary) $ \bs ->
              not $ any (BS.isPrefixOf bs) keys
  in
  genericProperty
    "'prefixLookup' returns 'Nothing' for non-prefix"
    ePrioTrie
    mkPrefixGen
    (const ())
    (const $ const isNothing)

-- ####################
-- ##    Helpers     ##
-- ####################

setMaxRatio :: Int -> Tasty.TestTree -> Tasty.TestTree
setMaxRatio maxRatio =
  Tasty.localOption (TQC.QuickCheckMaxRatio maxRatio)

scaleNumTests :: (Int -> Int) -> Tasty.TestTree -> Tasty.TestTree
scaleNumTests f =
  Tasty.adjustOption (\(TQC.QuickCheckTests v) -> TQC.QuickCheckTests $ f v)
