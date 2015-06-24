module App.Vandelay.Template.Types
  ( VandelayTemplate(..)
  , blankVandelayTemplate

  , Configuration(..)
  , blankConfiguration

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
  VandelayTemplate { --configuration :: Configuration
                     table         :: [TableCommand]
                   , substitutions :: [(Text, Text)] 
                   , desiredModels :: Last [String]
                   , texfile       :: Last String
                   , estimates     :: Last Estimates 
                   }
                deriving (Show)

blankVandelayTemplate = 
  VandelayTemplate { configuration = blankConfiguration
                   , table         = []
                   , substitutions = []
                   }
              
instance Monoid VandelayTemplate where
  mempty = blankVandelayTemplate
  mappend a b = 
    VandelayTemplate{ configuration = configuration a <> configuration b
                    , table         = table a <> table b
                    , substitutions = substitutions a <> substitutions b
                    }



-- loadEstimates :: VandelayTemplate -> EIO String VandelayTemplate
-- loadEstimates vt = do 
--   est    <- readEstimatesEIO =<< hoistEither (safeGetDatafile vt)
--   return vt{estimates = Last . Just $ est}


-- Configuration
data Configuration = 
  Configuration { desiredModels :: Last [String]
                , texfile       :: Last String
                , estimates     :: Last Estimates
                }
                deriving (Show)

blankConfiguration = Configuration (Last Nothing) (Last Nothing) (Last Nothing)


instance Monoid Configuration where
  mempty = blankConfiguration
  mappend ca cb   
    = Configuration { desiredModels = desiredModels ca <> desiredModels   cb
                    , texfile       = texfile  ca      <> texfile  cb
                    , estimates     = estimates ca     <> estimates cb
                    }

-- Table Commands
data TableCommand = Latex    String
                  | Template String
                  | Data     OutputRequest
                  deriving (Show)






safeGetEstimates     :: VandelayTemplate -> Either String Estimates
safeGetEstimates vt = 
  case (getLast . estimates . configuration$ vt) of 
    Nothing -> Left "No Estimate file specified"
    Just e  -> Right e

safeGetDesiredModels :: VandelayTemplate -> Either String [String] 
safeGetTexfile       :: VandelayTemplate -> Either String String 
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

