module App.Vandelay.Template.Types
  ( VandelayTemplate(..)
  , blankVandelayTemplate

  -- , Configuration(..)
  -- , blankConfiguration

  , TableCommand(..)

  , safeGetDesiredModels
  , safeGetTexfile
  , safeGetEstimates

  ) where

-- import App.Vandelay.Estimates
import App.Vandelay.Estimates.Types
import App.Vandelay.Estimates.ParserT
import App.Vandelay.Text
import App.Vandelay.Types


-- Vandalay Template 
data VandelayTemplate =
  VandelayTemplate { desiredModels :: Last [String]
                   , texfile       :: Last String
                   , estimates     :: [Estimates]
                   , table         :: [TableCommand]
                   , substitutions :: [(Text, Text)] 
                   }
                deriving (Show)

blankVandelayTemplate = 
  VandelayTemplate { desiredModels = Last Nothing
                   , texfile       = Last Nothing
                   , estimates     = []
                   , table         = []
                   , substitutions = []
                   }
              
instance Monoid VandelayTemplate where
  mempty = blankVandelayTemplate
  mappend a b = 
    VandelayTemplate{ desiredModels = desiredModels a <> desiredModels b
                    , texfile       = texfile a <> texfile b
                    , estimates     = estimates a <> estimates b
                    , table         = table a <> table b
                    , substitutions = substitutions a <> substitutions b
                    }


-- Table Commands
data TableCommand = Latex    String
                  | Template String
                  | Data     OutputRequest
                  deriving (Show)



-- Safe Accessors

safeGetDesiredModels :: VandelayTemplate -> Either String [String] 
safeGetTexfile       :: VandelayTemplate -> Either String String 
safeGetEstimates     :: VandelayTemplate -> Either String [Estimates]

safeGetDesiredModels  = safeGetFromTemplate desiredModels "Models not specified"
safeGetTexfile        = safeGetFromTemplate texfile "Output tex file not specified"

safeGetFromTemplate :: (VandelayTemplate -> Last a) -- accessor function
                       -> String -- error message
                       -> VandelayTemplate 
                       -> Either String a -- safe accessor
safeGetFromTemplate f e vt | unspecified = Left e
                           | otherwise   = Right $ fromJust d
  where unspecified = isNothing d
        d           = getLast . f $ vt

safeGetEstimates vt | null . estimates $ vt = Left "Estimate file not specified"
                    | otherwise             = Right $ estimates vt
