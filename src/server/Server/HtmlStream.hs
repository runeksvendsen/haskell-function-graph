{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server.HtmlStream
( HtmlStream
, Server.HtmlStream.toStream
, streamHtml
, Server.HtmlStream.liftStream
, streamTagBalanced, streamTagBalancedM
, streamTagBalancedAttr, streamTagBalancedAttrM
)
where

import Data.BalancedStream
import Lucid (Html)
import qualified Lucid.Base as Lucid
import qualified Data.Text as T
import Control.Monad.Trans.Class (MonadTrans)
import qualified Streaming.Prelude as S

-- | Example:
--
-- >>> import Lucid
-- >>> let stream = streamTagBalanced "html" <> streamTagBalanced "body" <> streamHtml (p_ "hello world")
-- >>> mconcat $ Data.Functor.Identity.runIdentity $ Streaming.Prelude.toList_ (toStream stream)
-- <html><body><p>hello world</p></body></html>
newtype HtmlStream m a = HtmlStream { unHtmlStream :: BalancedStream (Html ()) m a }
  deriving (Functor, Semigroup, Monoid, Applicative, Monad, MonadTrans)

toStream
  :: Monad m
  => HtmlStream m r
  -> S.Stream (S.Of (Html ())) m r
toStream = Data.BalancedStream.toStream . unHtmlStream

liftStream
  :: Monad m
  => S.Stream (S.Of (Html ())) m a
  -> HtmlStream m a
liftStream = HtmlStream . Data.BalancedStream.liftStream

-- | Stream a chunk of HTML that does not require an ending tag
streamHtml
  :: Monad m
  => Html ()
  -> HtmlStream m ()
streamHtml = HtmlStream . yield

mkStartEndElem
  :: T.Text
  -> (Lucid.Html (), Lucid.Html ()) -- ^ (start, end) tags
mkStartEndElem tag =
  (start, end)
  where
    start = Lucid.makeElementNoEnd tag
    end = Lucid.toHtmlRaw $ "</" <> tag <> ">"

-- | Stream a HTML tag that inserts an ending tag after subsequent 'streamHtml' chunks
streamTagBalanced
  :: Monad m
  => T.Text -- ^ tag
  -> HtmlStream m ()
streamTagBalanced tag = HtmlStream $
  uncurry yieldBalanced $ mkStartEndElem tag

-- | Same as 'streamTagBalanced' but accepts attributes
streamTagBalancedAttr
  :: Monad m
  => T.Text -- ^ tag
  -> [Lucid.Attribute]
  -> HtmlStream m ()
streamTagBalancedAttr tag attrs = HtmlStream $
  yieldBalanced (start `Lucid.with` attrs) end
  where
    (start, end) = mkStartEndElem tag

streamTagBalancedM
  :: Monad m
  => T.Text -- ^ tag
  -> HtmlStream m () -- ^ HTML inside tag
  -> HtmlStream m ()
streamTagBalancedM tag s = HtmlStream $
  yieldBalancedM start end (unHtmlStream s)
  where
    (start, end) = mkStartEndElem tag

-- | Same as 'streamTagBalanced' but accepts attributes
streamTagBalancedAttrM
  :: Monad m
  => T.Text -- ^ tag
  -> [Lucid.Attribute]
  -> HtmlStream m ()
  -> HtmlStream m ()
streamTagBalancedAttrM tag attrs s = HtmlStream $
  yieldBalancedM (start `Lucid.with` attrs) end (unHtmlStream s)
  where
    (start, end) = mkStartEndElem tag
