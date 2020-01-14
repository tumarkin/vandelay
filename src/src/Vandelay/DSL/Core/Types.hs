module Vandelay.DSL.Core.Types
  (

    -- * Estimates
    EstimatesHM
  , ModelHM
  , CoefHM

  , Cell
  , DataItem(..)

  , ModelName
  , CoefName


  , OutputRequest(..)
  , blankOutputRequest

  , oName
  , oCoeffs
  , oItemIdx
  , oFormat
  , oScale
  , oSurround
  , oEmpty
  , oModifyZero


  , texify

    -- * Output templates
  , VandelayTemplate(..)
  , blankVandelayTemplate

  , TableCommand(..)

  , getDesiredModels
  , getTexfile
  , getEstimatesHM

  ) where

import           Vandelay.DSL.Core.Modules
import           Vandelay.DSL.Core.Text

-- Estimates
type EstimatesHM = Map FilePath  ModelHM
type ModelHM     = Map ModelName CoefHM
type CoefHM      = Map CoefName  Cell

type ModelName    = Text
type CoefName     = Text

type Cell         = [DataItem]
data DataItem     = ValData  Double Significance
                  | StrData Text
                  | BlankData
                  deriving (Eq, Show, Ord)
type Significance = Int


-- Data Item
instance Monoid DataItem where
  mempty  = BlankData

  a         `mappend` BlankData = a
  BlankData `mappend` b         = b
  a         `mappend` b         = a




-- Vandalay Template
data VandelayTemplate = VandelayTemplate
    { desiredModels ∷ [(Maybe FilePath, Text)] -- ^ (Maybe Path, Model Name)
    , texfile       ∷ Maybe FilePath           -- ^ Output file path
    , estimatesHM   ∷ EstimatesHM
    , table         ∷ [TableCommand]
    , substitutions ∷ [(Text, Text)] } deriving (Show)

blankVandelayTemplate = VandelayTemplate
    { desiredModels = []
    , texfile       = Nothing
    , estimatesHM   = mempty
    , table         = []
    , substitutions = []
    }

instance Monoid VandelayTemplate where
  mempty = blankVandelayTemplate
  mappend a b = VandelayTemplate
      { desiredModels = desiredModels a <>  desiredModels b
      , texfile       = texfile a       <|> texfile b
      , estimatesHM   = estimatesHM a   <>  estimatesHM b
      , table         = table a         <>  table b
      , substitutions = substitutions a <>  substitutions b
      }


-- Table Commands
data TableCommand
    = Latex    Text
    | Template FilePath
    | Data     OutputRequest
    deriving (Show)



-- Safe Accessors
getDesiredModels ∷ VandelayTemplate → Either Text [(Maybe FilePath, Text)]
getTexfile       ∷ VandelayTemplate → Either Text FilePath
getEstimatesHM   ∷ VandelayTemplate → Either Text EstimatesHM

getDesiredModels vt
    | null . desiredModels $ vt = Left  "Models not specified"
    | otherwise                 = Right $ desiredModels vt

getTexfile =
    maybe (Left "Output tex file not specified") Right . texfile

getEstimatesHM vt
    | null . estimatesHM $ vt = Left "Estimate file not specified"
    | otherwise               = Right $ estimatesHM vt




-- Output request
data OutputRequest = OutputRequest
  { _oName       ∷ Text
  , _oCoeffs     ∷ [Text]
  , _oItemIdx    ∷ Int
  , _oFormat     ∷ String
  , _oSurround   ∷ (Text, Text)
  , _oScale      ∷ Double
  , _oModifyZero ∷ Bool -- ^ Modify zero (i.e. 0.000 to <0.001)
  , _oEmpty      ∷ Text
  } deriving (Show, Ord, Eq)

blankOutputRequest ∷ OutputRequest
blankOutputRequest = OutputRequest
  { _oName       = ""
  , _oCoeffs     = []
  , _oItemIdx    = 0
  , _oFormat     = "%1.3f"
  , _oSurround   = ("", "")
  , _oScale      = 1
  , _oModifyZero = True
  , _oEmpty      = ""
  }

makeLenses ''OutputRequest


-- Output functions
-- instance Latexable DataItem where
texify ∷ OutputRequest → DataItem → Text
texify _  (StrData t)   = "{" ++ t ++ "}"
texify or  BlankData    = or^.oEmpty
-- texify or (ValData v s) = changeAllZeros (surroundText or (commaPrintf (or^.oFormat) (or^.oScale * v))) ++ makeStars s
texify or (ValData v s) = surroundText st (changeAllZeros caz (commaPrintf fmt (scale * v))) ++ makeStars s
  where
    caz   = or^.oModifyZero
    st    = or^.oSurround
    fmt   = or^.oFormat
    scale = or^.oScale

makeStars ∷ Int → Text
makeStars i | i == 0    = ""
            | otherwise = "\\sym{" ++ replicate i '*' ++ "}"

surroundText ∷ (Text, Text) → Text → Text
surroundText (prefix, postfix) s = prefix ++ s ++ postfix

changeAllZeros ∷ Bool  -- ^ Modify all zeros to < (e.g. 0.000 to <0.001)
               → Text
               → Text
changeAllZeros False t = t
changeAllZeros True s = if   any (`elem` ['1'..'9']) s then s
                     else fixZero s

-- | Change 0.000 to $<$0.001 recognizing the number of decimal places
fixZero ∷ Text → Text
fixZero _s =
    "$<$" <> init s <> "1"
  where
    s = fromMaybe (error "fixZero on empty string") (fromNullable . strip $ _s)

