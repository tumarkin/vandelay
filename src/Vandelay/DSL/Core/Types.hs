{-# LANGUAGE TemplateHaskell #-}
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
  , FormatSpec(..)

  , oName
  , oCoeffs
  , oFormatSpec
  , oItemIdx
  , oFormat
  , oScale
  , oSurround
  , oEmpty
  , oModifyZero


  , texify

    -- * Output templates
  , VandelayTemplate(..)
  , TableCommand(..)

  , getDesiredModels
  , getEstimatesHM

  ) where

-- import           Lens.Simple               (makeLenses)
import           Data.Default.Class
import           Lens.Micro.TH
import qualified RIO.Text                  as T
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
instance Semigroup DataItem where
  a         <> BlankData = a
  BlankData <> b         = b
  a         <> b         = a

instance Monoid DataItem where
  mempty  = BlankData

-- Vandalay Template
data VandelayTemplate = VandelayTemplate
    { desiredModels ∷ [(FilePath, Text)] -- ^ (Maybe Path, Model Name)
    , estimatesHM   ∷ EstimatesHM
    , table         ∷ [TableCommand]
    , substitutions ∷ [(Text, Text)]
    } deriving (Show)

-- Table Commands
data TableCommand
    = Latex    Text
    -- | Template FilePath
    | Data     OutputRequest
    deriving (Show)



-- Safe Accessors
getDesiredModels ∷ VandelayTemplate → Either Text [(FilePath, Text)]
getEstimatesHM   ∷ VandelayTemplate → Either Text EstimatesHM

getDesiredModels vt
    | null . desiredModels $ vt = Left  "Models not specified"
    | otherwise                 = Right $ desiredModels vt

getEstimatesHM vt
    | null . estimatesHM $ vt = Left "Estimate file not specified"
    | otherwise               = Right $ estimatesHM vt




-- Output request
data OutputRequest = OutputRequest
  { _oName       ∷ !Text
  , _oCoeffs     ∷ ![Text]
  , _oFormatSpec ∷ !FormatSpec
  } deriving (Show, Ord, Eq)

instance Default OutputRequest where
    def = OutputRequest
      { _oName       = ""
      , _oCoeffs     = []
      , _oFormatSpec = def
      }

data FormatSpec = FormatSpec
  { _oItemIdx    :: !Int
  , _oFormat     :: !Text
  , _oSurround   :: !(Text, Text)
  , _oScale      :: !Double
  , _oModifyZero :: !Bool
  , _oEmpty      :: !(Maybe Text)
  } deriving (Show, Ord, Eq)

instance Default FormatSpec where
    def = FormatSpec
      { _oItemIdx    = 0
      , _oFormat     = "%1.3f"
      , _oSurround   = ("", "")
      , _oScale      = 1
      , _oModifyZero = True
      , _oEmpty      = Nothing
      }

makeLenses ''OutputRequest
makeLenses ''FormatSpec


-- Output functions
texify ∷ OutputRequest → DataItem → Text
texify _  (StrData t)   = "{" <> t <> "}"
texify or  BlankData    = fromMaybe "" $ or^.oFormatSpec.oEmpty
texify or (ValData v s) = surroundText st (changeAllZeros caz (commaPrintf fmt (scale * v))) <> makeStars s
  where
    caz   = or^.oFormatSpec.oModifyZero
    st    = or^.oFormatSpec.oSurround
    fmt   = T.unpack $ or^.oFormatSpec.oFormat
    scale = or^.oFormatSpec.oScale

makeStars ∷ Int → Text
makeStars i | i == 0    = ""
            | otherwise = "\\sym{" <> T.replicate i "*" <> "}"

