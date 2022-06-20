module Vandelay.App.Template.IO
  ( readTemplate
  ) where

import           Dhall
import           Dhall.Marshal.Decode as D
import           RIO
import qualified RIO.Text             as T
import           Vandelay.DSL.Core    as Core
import Vandelay.DSL.Estimates.ParserT

readTemplate ∷ FilePath → ExceptT ErrorMsg (RIO env) VandelayTemplate
readTemplate fp = 
     getEstimates =<< (liftIO $ inputFile vandelayTemplateDhall fp)

getEstimates ∷ VandelayTemplateDhall → ExceptT ErrorMsg (RIO env) VandelayTemplate
getEstimates vtdh = do
    ests <- mconcat <$> mapM readEstimates estimateFiles
    pure VandelayTemplate
      { desiredModels = first T.unpack <$> desiredModelsD vtdh
      , estimatesHM   = ests
      , table         = tableD vtdh
      , substitutions = []
      }
  where
    estimateFiles = T.unpack . fst <$> desiredModelsD vtdh


-- readEstimates' ∷ String → ExceptT ErrorMsg IO EstimatesHM
-- readEstimates' fp = readEstimates fp


data VandelayTemplateDhall = VandelayTemplateDhall
    { desiredModelsD ∷ [(Text, Text)] -- ^ (Maybe Path, Model Name)
    , tableD         ∷ [TableCommand]
    } deriving (Show)

vandelayTemplateDhall ∷ Decoder VandelayTemplateDhall
vandelayTemplateDhall = record
  ( VandelayTemplateDhall <$> field "models" (list desiredModel)
                          <*> field "table"  (list tableCommand)
  )

desiredModel ∷ Decoder (Text, Text)
desiredModel = record $ (,) <$> field "file" strictText <*> field "column" strictText

tableCommand ∷ Decoder TableCommand
tableCommand = union
  (  ( Latex <$> constructor "Latex" strictText)
  <> ( Data  <$> constructor "Row"   outputRequest)
  )

outputRequest ∷ Decoder OutputRequest
outputRequest = record $ OutputRequest
    <$> field "name" strictText
    <*> field "code" (list strictText)
    <*> field "format_spec" formatSpecD

formatSpecD ∷ Decoder FormatSpec
formatSpecD = record $ FormatSpec
    <$> (fromIntegral <$> field "index" natural)
    <*> field "format" strictText
    <*> pure ("", "")
    <*> field "scale" D.double
    <*> pure True
    <*> field "empty" (D.maybe strictText)

test ∷ IO ()
test = runSimpleApp $ do
    let fp = "/Users/roberttumarkin/Coding/research_templates/paper/tables/3.oof.dhall"
    vt <- liftIO $ inputFile vandelayTemplateDhall fp
    logInfo . displayShow $ vt

-- readEstimates ∷ oncat <$> mapM readEstimates es
--
--                  ⇒ String  -- File name
--                  → m EstimatesHM


-- data TableCommand'
--   = Latex' Text
--   | Row' Core.OutputRequest
--   deriving (Show)

-- translateTableCommand ∷ TableCommand' → Core.TableCommand
-- translateTableCommand (Latex' t ) = Core.Latex t
-- translateTableCommand (Row' rs) = undefined -- Core.Data Core.OutputRequest
  --   { Core._oName       = name rs
  --   , Core._oCoeffs     = code rs
  --   , Core._oItemIdx    = fromIntegral $ itemIdx fs
  --   , Core._oFormat     = T.unpack $ format fs
  --   , Core._oSurround   = ("", "")
  --   , Core._oScale      = scale fs
  --   , Core._oModifyZero = True
  --   , Core._oEmpty      = fromMaybe "" $ empty fs
  --   }
  -- where fs = formatSpec rs

-- data RowSpec = RowSpec
--   { name       :: !Text
--   , code       :: ![Text]
--   , formatSpec :: !FormatSpec
--   } deriving (Show)

