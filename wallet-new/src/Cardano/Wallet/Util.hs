
-- | Module for small utility functions.
module Cardano.Wallet.Util
       ( -- * String manipulation
         headToLower
       , stripFieldPrefix
       , mkJsonKey

       -- * Time
       , defaultApiTimeLocale
       , apiTimeFormat
       , parseApiUtcTime
       , showApiUtcTime
       ) where

import           Universum

import           Data.Char (isUpper, toLower)
import qualified Data.Time as Time

-- * String manipulation utils

headToLower :: String -> Maybe String
headToLower []     = Nothing
headToLower (x:xs) = Just $ toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

mkJsonKey :: String -> String
mkJsonKey s = fromMaybe s . headToLower $ stripFieldPrefix s

-- * Time

-- | Currently we support only American usage.
defaultApiTimeLocale :: Time.TimeLocale
defaultApiTimeLocale = Time.defaultTimeLocale

-- | Time format used in API. Corresponds to ISO-8601.
-- Example of time: "2018-03-07T16:20:27.477318"
--
-- Note: there is more neat support of time formats in time-1.9,
-- Data.Time.Format.ISO8601 module, but that version is barely applicable with
-- current LTS-9.1.
apiTimeFormat :: String
apiTimeFormat = Time.iso8601DateFormat (Just "%H:%M:%S%Q")

-- | Parse UTC time from API.
parseApiUtcTime :: MonadFail m => Text -> m Time.UTCTime
parseApiUtcTime = Time.parseTimeM False defaultApiTimeLocale apiTimeFormat . toString

-- | Encode UTC time for API.
showApiUtcTime :: Time.UTCTime -> Text
showApiUtcTime = toText . Time.formatTime defaultApiTimeLocale apiTimeFormat
