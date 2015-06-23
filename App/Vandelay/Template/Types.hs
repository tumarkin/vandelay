module App.Vandelay.Template.Types
  ( VandelayTemplate(..)
  , blankVandelayTemplate
  , loadEstimates

  , Configuration(..)
  , blankConfiguration

  , TableCommand(..)

  , safeGetDatafile
  , safeGetDesiredModels
  , safeGetTexfile


  ) where

-- import App.Vandelay.Estimates
import App.Vandelay.Estimates.Types
import App.Vandelay.Estimates.Parser
import App.Vandelay.Text
import App.Vandelay.Types


-- Vandalay Template 
data VandelayTemplate =
  VandelayTemplate { configuration :: Configuration
                   , table         :: [TableCommand]
                   , substitutions :: [(Text, Text)] 
                   , estimates     :: Maybe Estimates
                   }
                deriving (Show)

blankVandelayTemplate = 
  VandelayTemplate { configuration = blankConfiguration
                   , table         = []
                   , substitutions = []
                   , estimates     = Nothing
                   }
              
instance Monoid VandelayTemplate where
  mempty = blankVandelayTemplate
  mappend a b = 
    VandelayTemplate{ configuration = configuration a <> configuration b
                    , table         = table a <> table b
                    , substitutions = substitutions a <> substitutions b
                    , estimates     = Nothing
                    }



loadEstimates :: VandelayTemplate -> EIO String VandelayTemplate
loadEstimates vt = do 
  est    <- readEstimatesEIO =<< hoistEither (safeGetDatafile vt)
  return vt{estimates = Just $ est}


-- Configuration
data Configuration = 
  Configuration { datafile      :: Last String
                , desiredModels :: Last [String]
                , texfile       :: Last String
                }
                deriving (Show)

blankConfiguration = Configuration (Last Nothing) (Last Nothing) (Last Nothing)


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







safeGetDatafile      :: VandelayTemplate -> Either String String 
safeGetDesiredModels :: VandelayTemplate -> Either String [String] 
safeGetTexfile       :: VandelayTemplate -> Either String String 
safeGetDatafile       = safeGetFromConfiguration datafile "Data file not specified"
safeGetDesiredModels  = safeGetFromConfiguration desiredModels "Models not specified"
safeGetTexfile        = safeGetFromConfiguration texfile "Output tex file not specified"

safeGetFromConfiguration :: (Configuration -> Last a) -- accessor function
                         -> String -- error message
                         -> VandelayTemplate 
                         -> Either String a -- safe accessor
safeGetFromConfiguration f e vt | unspecified = Left $ e
                                | otherwise   = Right $ fromJust d
  where unspecified = isNothing d
        d           = getLast . f . configuration $ vt

