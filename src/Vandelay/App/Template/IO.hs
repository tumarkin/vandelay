module Vandelay.App.Template.IO
  ( readTemplate
  ) where

import           Dhall
import           Dhall.Marshal.Decode           as D
import           RIO
import qualified RIO.Text                       as T

import           Vandelay.DSL.Core              as Core
import           Vandelay.DSL.Estimates.ParserT

readTemplate ∷ FilePath → ExceptT ErrorMsg (RIO env) VandelayTemplate
readTemplate fp =
     getEstimates =<< (liftIO . inputFile vandelayTemplateDhall $ fp)

getEstimates ∷ VandelayTemplateDhall → ExceptT ErrorMsg (RIO env) VandelayTemplate
getEstimates vtdh = do
    ests <- mconcat <$> mapM readEstimates estimateFiles
    pure VandelayTemplate
      { desiredModels = first T.unpack <$> desiredModelsD vtdh
      , estimatesHM   = ests
      , table         = tableD vtdh
      , substitutions = []
      , target        = targetD vtdh
      }
  where
    estimateFiles = T.unpack . fst <$> desiredModelsD vtdh

data VandelayTemplateDhall = VandelayTemplateDhall
    { desiredModelsD ∷ [(Text, Text)] -- ^ (Maybe Path, Model Name)
    , tableD         ∷ [TableCommand]
    , targetD        ∷ Target
    } deriving (Show)

vandelayTemplateDhall ∷ Decoder VandelayTemplateDhall
vandelayTemplateDhall = record
  ( VandelayTemplateDhall <$> field "models" (list desiredModel)
                          <*> field "table"  (list tableCommand)
                          <*> field "target" undefined
  )

desiredModel ∷ Decoder (Text, Text)
desiredModel = record $ (,) <$> field "file" strictText <*> field "column" strictText

tableCommand ∷ Decoder TableCommand
tableCommand = union
  (  ( Raw <$> constructor "Raw" strictText)
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
    <*> field "surround" (D.maybe surroundD)
    <*> field "scale" D.double
    <*> field "reformat_zero" D.bool
    <*> field "empty" (D.maybe strictText)

surroundD ∷ Decoder (Text, Text)
surroundD = record $ (,) <$> field "before" strictText <*> field "after" strictText

