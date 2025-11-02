-- | Internal module.
--
-- @since 0.1
module Kairos.Internal
  ( -- * TZ Database Labels
    tzNameToLabel,
    tzLabelToName,
    tzLowerNameLabelMap,
    tzLowerNameLabelMapWith,
  )
where

import Control.Applicative (asum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
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
tzNameToLabel :: Text -> Maybe TZLabel
tzNameToLabel = (`Map.lookup` tzLowerNameLabelMap) . T.toLower

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

-- | Renders a 'TZLabel' to a 'Text' that is parseable by 'tzNameToLabel'.
-- Note that this depends on the bundled TZ database, so while we do test
-- this function, there could be discrepancies with different tz version.
--
-- The stakes are pretty low, as this function is only used to supply tab
-- completions. So the worst case scenario is tab-completing a tz that
-- is not right.
tzLabelToName :: TZLabel -> Text
tzLabelToName =
  -- First, we replace all '__' with '/', since that is a universal change.
  -- After that we try several replace functions, fixing strings on a
  -- case-by-case basis.
  fixSpecialCases
    . fixDoubleUs
    . T.pack
    . show
  where
    fixDoubleUs = T.replace "__" "/"
    fixGmtP = T.replace "GMT'" "GMT+"
    fixGmtS = T.replace "GMT_" "GMT-"

    -- We run a list of replacement functions that return Just if the
    -- replacement is a match, otherwise returning Nothing. The idea is that
    -- each string should match at most one replacement function. If not
    -- function matches, just return the string as-is, as presumably no
    -- replacement is necessary.
    fixSpecialCases t =
      fromMaybe t $
        asum $
          fmap
            ($ t)
            [ -- Fix GMT labels
              runReplace fixGmtP,
              runReplace fixGmtS,
              -- Map '_' to '-' e.g. "Africa/Porto_Novo" -> "Africa/Porto-Novo".
              -- There are more labels that do /not/ replace underscores with
              -- hyphens, hence we have to special case the ones that do.
              usToHyphen
            ]

    -- Runs the replacement function. If a change was detected, returns
    -- Just the result. Otherwise Nothing.
    runReplace :: (Text -> Text) -> Text -> Maybe Text
    runReplace f t =
      let s = f t
       in if s == t
            then Nothing
            else Just s

    usToHyphen t =
      if Set.member t hyphens
        then Just $ T.replace "_" "-" t
        else Nothing

    hyphens =
      Set.fromList
        [ "Africa/Porto_Novo",
          "America/Blanc_Sablon",
          "America/Port_au_Prince",
          "Asia/Ust_Nera",
          "GB_Eire",
          "NZ_CHAT",
          "US/East_Indiana",
          "US/Indiana_Starke",
          "W_SU"
        ]
