-- | Internal module.
--
-- @since 0.1
module Kairos.Internal
  ( -- * TZ Database Labels
    tzNameToTZLabel,
    tzLowerNameLabelMap,
    tzLowerNameLabelMapWith,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (OnDecodeError)
import Data.Text.Encoding.Error qualified as TError
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.All qualified as All

-- | Looks up a tz database label by name. Case-insensitive.
--
-- @since 0.1
tzNameToTZLabel :: Text -> Maybe TZLabel
tzNameToTZLabel = (`Map.lookup` tzLowerNameLabelMap) . T.toLower

-- | Like @tz@'s @tzNameLabelMap@ but with lower-case 'Text' keys instead of
-- ByteString for case-insensitive lookup.
--
-- @since 0.1
tzLowerNameLabelMap :: Map Text TZLabel
tzLowerNameLabelMap = tzLowerNameLabelMapWith TError.lenientDecode

-- | tzLowerNameLabelMapOnError with custom decoder, since we need to convert
-- ByteString -> Text.
--
-- @since 0.1
tzLowerNameLabelMapWith :: OnDecodeError -> Map Text TZLabel
tzLowerNameLabelMapWith decoder = Map.mapKeys toLower All.tzNameLabelMap
  where
    toLower = T.toLower . TEnc.decodeUtf8With decoder
