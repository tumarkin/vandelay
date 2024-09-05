module Vandelay.App.Template.IO (
    readTemplate,
) where

import Control.Monad.Except
import Dhall
import Dhall.Marshal.Decode as D
import RIO
import qualified RIO.Text as T

import Vandelay.DSL.Core as Core
import Vandelay.DSL.Estimates.ParserT

readTemplate ∷ FilePath → ExceptT ErrorMsg (RIO env) VandelayTemplate
readTemplate fp =
    getEstimates =<< (liftIO . inputFile vandelayTemplateDhall $ fp)

getEstimates ∷ VandelayTemplateDhall → ExceptT ErrorMsg (RIO env) VandelayTemplate
getEstimates vtdh = do
    ests ← mconcat <$> mapM readEstimates estimateFiles
    pure
        VandelayTemplate
            { desiredModels = first T.unpack <$> vtdh.desiredModelsD
            , estimatesHM = ests
            , table = vtdh.tableD
            , target = vtdh.targetD
            }
  where
    estimateFiles = T.unpack . fst <$> vtdh.desiredModelsD

data VandelayTemplateDhall = VandelayTemplateDhall
    { desiredModelsD ∷ [(Text, Text)]
    -- ^ (Path, Model Name)
    , tableD ∷ [TableCommand]
    , targetD ∷ Target
    }
    deriving (Show)

vandelayTemplateDhall ∷ Decoder VandelayTemplateDhall
vandelayTemplateDhall =
    record
        ( VandelayTemplateDhall
            <$> field "models" (list desiredModel)
            <*> field "table" (list tableCommand)
            <*> field "target" targetD
        )

desiredModel ∷ Decoder (Text, Text)
desiredModel = record $ (,) <$> field "file" strictText <*> field "column" strictText

tableCommand ∷ Decoder TableCommand
tableCommand =
    union
        ( (Raw <$> constructor "Latex" strictText)
       <> (Data <$> constructor "Row" outputRequest)
        )

outputRequest ∷ Decoder OutputRequest
outputRequest =
    record
        $ OutputRequest
        <$> field "name" strictText
        <*> field "code" (list strictText)
        <*> field "format_spec" formatSpecD

formatSpecD ∷ Decoder FormatSpec
formatSpecD =
    record
        $ FormatSpec
        <$> (fromIntegral <$> field "index" natural)
        <*> field "format" strictText
        <*> field "surround" (D.maybe surroundD)
        <*> field "scale" D.double
        <*> field "reformat_zero" D.bool
        <*> field "empty" (D.maybe strictText)

surroundD ∷ Decoder (Text, Text)
surroundD = record $ (,) <$> field "before" strictText <*> field "after" strictText

targetD :: Decoder Target
targetD = union $
    (LatexTarget <$ constructor "Latex" unit)
    <> (TypstTarget <$ constructor "Typst" unit)
