module Vandelay.DSL.Estimates.Output
  ( outputRow
  ) where

import           Data.NonNull
import           Data.Sequences    (index)
import qualified RIO.List          as L
import qualified RIO.Map           as M
import qualified RIO.Text          as T

import           Vandelay.DSL.Core

-- | Output a row
outputRow ∷ Target
          → OutputRequest
          → EstimatesHM
          → [(FilePath, ModelName)]
          → Either ErrorMsg Text
outputRow _tgt or est ms = do
  result <- mapM _findDataItem ms
  Right $ joinAmps ( or^.oName : -- Name
                     map (texify or) result
                   )
          <> "\\\\"

  where
    _findDataItem ∷ (FilePath, ModelName) → Either ErrorMsg DataItem
    _findDataItem mn = findDataItem mn (or^.oCoeffs) (or^.oFormatSpec.oItemIdx) est

findDataItem ∷ (FilePath, ModelName) -- ^ Estimates file, ModelName
             → [Text]                -- ^ Possible coefficients
             → Int                   -- ^ Item index
             → EstimatesHM
             → Either ErrorMsg DataItem
findDataItem (efp, mn) pCoefs idx est = do

    -- Find estimates. Match required when the file is specified, unique
    -- named estimates when the file is not specified.
    emsInFiles <- do
        let matchingEst = M.filterWithKey (\k _ → k == efp) est
        if | length matchingEst == 0 → Left $ noEstFileErr efp
           | length matchingEst == 1 → Right . head . impureNonNull $ matchingEst
           | otherwise               → Left $ multiEstFileErr efp

    -- Find model in estimates
    let modelHMs   = M.filterWithKey (\k _ → k == mn) emsInFiles
    coefHM <- if | length modelHMs == 0 → Left noModelErr
                 | length modelHMs == 1 → Right . head . impureNonNull $ modelHMs
                 | otherwise            → Left multiModelErr

    -- Find all coefficient maps that match the requested data
    let cells ∷ Map CoefName Cell
        cells = M.filterWithKey (\k _ → k `elem` pCoefs) coefHM

    -- Ensure that at least one coefficient was found
    when (null cells && "missing" `notElem` pCoefs) $ Left noCoefferr

    -- Find all references dataitem
    let dis ∷ [DataItem]
        dis = mapMaybe (`index` idx) (toList cells)


    -- Select dataitem, prioritizing intrinsic ordering of value, then string, then blank
    case fromNullable . L.sort $ dis of
      Just nd → Right $ head nd
      Nothing → if   "missing" `elem` pCoefs
                then Right BlankData
                else Left $ noIndexErr (M.keys coefHM)

  where

    noEstFileErr ∷ FilePath → Text
    noEstFileErr ef = T.unwords [ "Estimates file", T.pack ef, "not found in data files"
                              , unwordEnglishList tSourceFiles
                              ]

    multiEstFileErr ∷ FilePath → Text
    multiEstFileErr ef = T.unwords [ "Estimates path", T.pack ef
                                 , "matches multiple data files:"
                                 ]

    noModelErr ∷ Text
    noModelErr = T.unwords [ "Model", mn, "not found in data files"
                         , unwordEnglishList tSourceFiles
                         ]

    multiModelErr ∷ Text
    multiModelErr = T.unwords [ "Model", mn
                            , "found in multiple data files."
                            , "Use explicit FILENAME:MODEL syntax"
                            ]

    -- noCoefErr ∷ Text
    -- noCoefErr = T.unwords [ "Coefficients", unwordEnglishList pCoefs
    --                     , "not found in estimate", mn
    --                     ]

    noCoefferr ∷ Text
    noCoefferr = T.unwords
      [ "Coefficients", unwordEnglishList pCoefs
      , "not found in specification", mn,
      "\nAdd 'missing' to coefficient list to allow for missing data."]

    noIndexErr  ∷ [Text] → Text
    noIndexErr cfs = T.unwords
      [ "No data item found for model ", mn
      , "coefficient", unwordEnglishList cfs
      , ", at index", tshow idx
      ]

    tSourceFiles ∷ [Text]
    tSourceFiles = map T.pack $ M.keys est

-- validateUnique ∷ ModelName → EstimatesHM → Either Text ModelHM
-- validateUnique mn est =
--     if   mn `notElem` repeatedModels
--     then Right $ M.unions . toList $ est
--     else Left  $ unwords ["Model", mn, "repeated in estimates file."
--                          , "Use explicit FILENAME:MODEL syntax"
--                          ]
--   where
--     modelCounts ∷ Counter ModelName
--     modelCounts = count . concatMap M.keys . toList $ est

--     repeatedModels ∷ [ModelName]
--     repeatedModels = map fst . filter ((> 1) . snd) . listifyCounter $ modelCounts

