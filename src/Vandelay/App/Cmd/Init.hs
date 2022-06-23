{-# LANGUAGE QuasiQuotes   #-}
module Vandelay.App.Cmd.Init
  ( initTemplate
  ) where

import           Data.NonNull
import           Data.String.Interpolate (i)
import           RIO.FilePath
import qualified RIO.Set                 as Set
import qualified RIO.Text                as T
import           Vandelay.DSL.Core

-- Initialize template
initTemplate ∷ [FilePath]                         -- ^ Estimation results filepaths
             → Maybe FilePath                     -- ^ Optional output file (stdout if nothing)
             -- → SortOptions
             → ExceptT ErrorMsg (RIO env) ()      -- ^ Error message or ()
initTemplate estGlobs textOutFile = do
  globs      <- globPaths estGlobs

  -- Files and contents [(FilePath, Text)]
  estFilePathContents <- mapM (\fp -> (fp,) <$> safeReadFile fp) globs
  unsafeWriteFile textOutFile (template estFilePathContents)


template
    ∷ [(FilePath, Text)] -- ^ File paths
    → Text
template ests = [i|
----------------------------------------------------------------------------------------------------
-- Import library                                                                                 --
----------------------------------------------------------------------------------------------------
let vl = ./vandelay/module.dhall

let row              = vl.command.row
let latex            = vl.command.latex
let latex_columns_ln = vl.command.latex_columns_ln

----------------------------------------------------------------------------------------------------
-- Model configuration                                                                            --
----------------------------------------------------------------------------------------------------
#{filesText}

let Model = { file: Text, column: Text }

let models =
    [ #{modelText}
    ]


----------------------------------------------------------------------------------------------------
-- Substitutions                                                                                  --
----------------------------------------------------------------------------------------------------
let caption     =  "TITLE"
let label       =  "tbl:label"
let font_size   =  "small"
let header_size =  "small"
let header_text = ''
    The table reports results for fixed effects models examining
    the relations ...
    %
    Standard errors are ...
    %
    \\textit{t}-statistics are reported in parentheses.
    %
    Coefficients marked with ***, **, and * are significant at
    the 1\\%, 5\\% and 10\\% level, respectively.
    ''

let table_header = ''
    \\midrule
    ''

----------------------------------------------------------------------------------------------------
-- Table                                                                                          --
----------------------------------------------------------------------------------------------------
-- Configuration
let number_of_models = List/length Model models
let column_specification = vl.column_spec 32.0 number_of_models 19.9

let header_config = {caption, column_specification, font_size, header_size, header_text, label, table_header}
let pb_config     = {column_specification, font_size, number_of_models, table_header}

-- Custom Functions
let base_format     = { index = 0, format = "%03.2f", scale = 1.0, empty = None Text }
let tstat_format    = base_format with index = 1
let line_space      = latex "\\addlinespace[1pt]"

let coefficient_and_tstat = \\(cname: Text) -> \\(tname: Text) -> \\(code : List Text) ->
   [ row base_format cname code
   , row tstat_format tname code
   , line_space
   ]

let section_describe = \\(desc : Text) ->
    [ line_space
    , latex ("\\textit{" ++ desc ++ ":}\\\\")
    ]

-- Table specification
let fixed_effects_clustering =
    [ latex_columns_ln ["FE"]
    , latex_columns_ln ["Clustering"]
    ]

#{variableText}

let table =
    vl.header header_config
    \# section_descripte "CONTROL HEADER"
    \# coefficient_and_tstat "VARIABLE NAME"        "T-STAT NAME"             ["CODE-1", "CODE-2"]
    \# vl.page_break pb_config
    \# [line_space]
    \# fixed_effects_clustering
    \# vl.footer

in { models
   , table
   }
|]

  where
    filesText ∷ Text
    filesText = T.unlines $ map (\(fp, abbrev, _) -> abbrev <> " = " <> quote fp) fpAbbrevModels


    fpAbbrevModels ∷ [(Text, Text, [Text])]
    fpAbbrevModels = zipWith fpAbbrevModel ests ['a'..]

    fpAbbrevModel ∷ (FilePath, Text) → Char → (Text, Text, [Text])
    fpAbbrevModel (fp, content) fileCode = (T.pack fp, fileAbbrev fileCode, modelsFromContent content)

    fileAbbrev c = "est_" <> T.singleton c


    modelText ∷ Text
    modelText = T.intercalate "\n    , " $ concatMap makeModels fpAbbrevModels
      where
        makeModels (_, abbrev, models) =
            map (\m -> "(" <> abbrev <> ", " <> quote m <> ")") models

    quote t = "\"" <> t <> "\""

    variableText ∷ Text
    variableText = T.unlines . Set.toAscList . Set.fromList . fmap indentAndComment $ concatMap (varsFromContent . snd) ests

----------------------------------------------------------------------------------------------------
-- Utility functions                                                                              --
----------------------------------------------------------------------------------------------------

-- | Model and variable from content
modelsFromContent ∷ Text   -- | Content
                  → [Text] -- | Model
modelsFromContent t =
  maybe []
        (stripFilter . splitOn tab . head)
        (fromNullable . T.lines $ t)


varsFromContent ∷ Text   -- | Content
                → [Text] -- | [(Variable, Ref)]
varsFromContent =
  stripFilter . mapMaybe (fmap head . fromNullable . splitOn tab) . T.lines

-- | Text utility functions
stripFilter∷ [Text] → [Text] -- | Remove spaces, drop blanks
stripFilter = filter (not . T.null) . map T.strip

tab ∷ Text
tab = "\t"

indent∷ Text → Text
indent = (<>) "    "

indentAndComment ∷ Text → Text
indentAndComment = indent . (<>) "-- "

