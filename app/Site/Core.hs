--------------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------------------

module Site.Core
  ( module Hakyll
  , UTCTime
  , getTitle
  , ToContext(..)
  , (<+>)
  , traverseToSnd
  ) where

--------------------------------------------------------------------------------

import           Data.Time       (UTCTime)
import           Hakyll
import           System.FilePath (takeBaseName)

--------------------------------------------------------------------------------

class ToContext a where
  toContext :: a -> Context b

--------------------------------------------------------------------------------

getTitle :: Item a -> String
getTitle = takeBaseName . toFilePath . itemIdentifier

--------------------------------------------------------------------------------

infixr 6 <+>
(<+>) :: (Monoid a, Applicative m) => a -> m a -> m a
a <+> ma = mappend a <$> ma

--------------------------------------------------------------------------------

traverseToSnd :: Functor t => (a -> t b) -> a -> t (a, b)
traverseToSnd f a = (a,) <$> f a

--------------------------------------------------------------------------------
