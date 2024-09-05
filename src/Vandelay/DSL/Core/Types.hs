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

    -- * Output templates
  , VandelayTemplate(..)
  , TableCommand(..)
  , Target

  , getDesiredModels
  , getEstimatesHM

  ) where

import           Data.Default.Class
import RIO

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
    , target        ∷ Target
    } deriving (Show)

data Target = LatexTarget | TypstTarget
  deriving (Show)

-- Table Commands
data TableCommand
    = Raw Text
    -- | Template FilePath
    | Data OutputRequest
    deriving (Show)



-- Safe Accessors
getDesiredModels ∷ VandelayTemplate → Either Text [(FilePath, Text)]
getEstimatesHM   ∷ VandelayTemplate → Either Text EstimatesHM

getDesiredModels vt
    | null vt.desiredModels = Left  "Models not specified"
    | otherwise             = Right vt.desiredModels 

getEstimatesHM vt
    | null vt.estimatesHM = Left "Estimate file not specified"
    | otherwise           = Right vt.estimatesHM 




-- Output request
data OutputRequest = OutputRequest
  { name       ∷ !Text
  , coeffs     ∷ ![Text]
  , formatSpec ∷ !FormatSpec
  } deriving (Show, Ord, Eq)

instance Default OutputRequest where
    def = OutputRequest
      { name       = ""
      , coeffs     = []
      , formatSpec = def
      }

data FormatSpec = FormatSpec
  { itemIdx    :: !Int
  , format     :: !Text
  , surround   :: !(Maybe (Text, Text))
  , scale      :: !Double
  , modifyZero :: !Bool
  , empty      :: !(Maybe Text)
  } deriving (Show, Ord, Eq)

instance Default FormatSpec where
    def = FormatSpec
      { itemIdx    = 0
      , format     = "%1.3f"
      , surround   = Nothing
      , scale      = 1
      , modifyZero = True
      , empty      = Nothing
      }

