module App.Vandelay.Template.Types
  ( VandelayTemplate(..)
  , blankVandelayTemplate

  , VTLoaded(..)
  , toVTLoaded

  , Configuration(..)
  , blankConfiguration

  , TableCommand(..)

  -- , DataCommand(..)

  , safeGetDatafile
  , safeGetDesiredModels
  , safeGetTexfile

  -- , createOutputRequest
  -- , OutputRequest(..)
  ) where

import App.Vandelay.Estimates
import App.Vandelay.Estimates.Types
import App.Vandelay.Text
import App.Vandelay.Types

import Debug.Trace




-- Vandalay Template 

data VandelayTemplate =
  VandelayTemplate { configuration :: Configuration
                   , table         :: [TableCommand]
                   , substitutions :: [(Text, Text)] 
                   }
                deriving (Show)

blankVandelayTemplate = 
  VandelayTemplate { configuration = blankConfiguration
                   , table         = []
                   , substitutions  = []
                   }
              
instance Monoid VandelayTemplate where
  mempty = blankVandelayTemplate
  mappend a b = 
    VandelayTemplate{ configuration = configuration a <> configuration b
                    , table         = table a <> table b
                    , substitutions  = substitutions a <> substitutions b
                    }


-- Klugey fix for vandelay template loading which should be done in the parser
data VTLoaded =
  VTLoaded{ vtlTable         :: [TableCommand]
          , vtlSubstitutions :: [(Text, Text)] 
          , vtlDesiredModels :: [String]
          , vtlEstimates     :: Estimates
          , vtlOutputKluge   :: (Estimates, [String])
          }
          deriving (Show)

toVTLoaded :: VandelayTemplate -> EIO String VTLoaded
toVTLoaded vt = do
  config <- return (configuration vt)
  dms    <- hoistEither (safeGetDesiredModels config)
  df     <- hoistEither (safeGetDatafile config)
  est    <- readEstimatesEIO (df)

  return VTLoaded{ vtlTable         = table vt
                 , vtlSubstitutions = substitutions vt
                 , vtlDesiredModels = dms
                 , vtlEstimates     = est
                 , vtlOutputKluge   = (est, dms)
                 }





-- Configuration
data Configuration = 
  Configuration { datafile      :: Last String
                , desiredModels :: Last [String]
                , texfile       :: Last String
                }
                deriving (Show)

blankConfiguration = Configuration (Last Nothing) (Last Nothing) (Last Nothing)


safeGetDatafile      :: Configuration -> Either String String 
safeGetDesiredModels :: Configuration -> Either String [String] 
safeGetTexfile       :: Configuration -> Either String String 
safeGetDatafile       = safeGetFromConfiguration datafile "Data file not specified"
safeGetDesiredModels  = safeGetFromConfiguration desiredModels "Models not specified"
safeGetTexfile        = safeGetFromConfiguration texfile "Output tex file not specified"

safeGetFromConfiguration :: (Configuration -> Last a) -- accessor function
                         -> String -- error message
                         -> Configuration 
                         -> Either String a -- safe accessor
safeGetFromConfiguration f e c | unspecified = Left $ e
                               | otherwise   = Right $ fromJust d
  where unspecified = isNothing d
        d           = getLast . f $ c 







instance Monoid Configuration where
  mempty = blankConfiguration
  mappend ca cb   
    = Configuration { datafile      = datafile ca      <> datafile cb
                    , desiredModels = desiredModels ca <> desiredModels   cb
                    , texfile       = texfile  ca      <> texfile  cb
                    }





-- Table Commands
data TableCommand = Latex    String
                  | Template String
                  | Data     OutputRequest
                  deriving (Show)
