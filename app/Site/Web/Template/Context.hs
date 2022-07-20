module Site.Web.Template.Context (modificationDateField) where

import Control.Monad (msum)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (formatTime, parseTimeM)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import Hakyll.Core.Identifier
import Hakyll.Core.Item
import Hakyll.Core.Metadata
import Hakyll.Web.Template.Context
import Prelude hiding (id)

modificationDateField :: String -> String -> Context a
modificationDateField = modificationDateFieldWith defaultTimeLocale

modificationDateFieldWith :: TimeLocale -> String -> String -> Context a
modificationDateFieldWith locale key format = field key $ \i -> do
  time <- getItemUpdateUTC locale $ itemIdentifier i
  return $ formatTime locale format time

getItemUpdateUTC :: (MonadMetadata m, MonadFail m) => TimeLocale -> Identifier -> m UTCTime
getItemUpdateUTC locale id' = do
  metadata <- getMetadata id'
  let tryField k fmt = lookupString k metadata >>= parseTime' fmt

  maybe empty' return $
    msum $
      [tryField "update" fmt | fmt <- formats]
  where
    empty' =
      fail $
        "Site.Web.Template.Context.getItemUpdateUTC: "
          ++ "could not parse time for "
          ++ show id'
    parseTime' = parseTimeM True locale
    formats =
      [ "%a, %d %b %Y %H:%M:%S %Z",
        "%a, %d %b %Y %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S%Z",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M:%S%Z",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d",
        "%B %e, %Y %l:%M %p",
        "%B %e, %Y",
        "%b %d, %Y"
      ]
