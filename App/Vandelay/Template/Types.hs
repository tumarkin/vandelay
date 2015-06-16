module App.Vandelay.Template.Types
  ( VandelayTemplate(..)
  , blankVandelayTemplate

  , Configuration(..)
  , blankConfiguration

  , TableCommand(..)
  , DataCommand(..)

  , safeGetDatafile
  , safeGetDesiredModels
  , safeGetTexfile

  , createOutputRequest
  , OutputRequest(..)
  ) where

import Data.Maybe
import Data.Monoid
import App.Vandelay.Estimates.Types
import App.Vandelay.Text




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
    VandelayTemplate{ configuration = configuration a `mappend` configuration b
                 , table         = table a `mappend` table b
                 , substitutions  = substitutions a `mappend` substitutions b
                 }






-- Configuration
data Configuration = 
  Configuration { datafile      :: Last Text
                , desiredModels :: Last Text
                , texfile       :: Last Text
                }
                deriving (Show)

blankConfiguration = Configuration (Last Nothing) (Last Nothing) (Last Nothing)


safeGetDatafile      :: Configuration -> Either String Text 
safeGetDesiredModels :: Configuration -> Either String Text 
safeGetTexfile       :: Configuration -> Either String Text 
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
    = Configuration { datafile = datafile ca `mappend` datafile cb
                    , desiredModels   = desiredModels   ca `mappend` desiredModels   cb
                    , texfile  = texfile  ca `mappend` texfile  cb
                    }





-- Table Commands

data TableCommand = Latex    Text
                  | Template Text
                  | Data     OutputRequest
                  deriving (Show)

data DataCommand = Name     Text
                 | Code     Text
                 | Index    Text
                 | Format   Text
                 | Scale    Text
                 | Surround Text
                 deriving (Show)









createOutputRequest :: [DataCommand] 
                    -> OutputRequest
createOutputRequest dcs = 
  foldl modifyOutputRequest defaultOutputRequest dcs 

modifyOutputRequest :: OutputRequest -> DataCommand -> OutputRequest
modifyOutputRequest or (Name     t) = or{oName    = unpack t}
modifyOutputRequest or (Code     t) = or{oCoeffs  = stripSplitCommas t}
modifyOutputRequest or (Index    t) = or{oItemIdx = (read . unpack $ t)::Int}
modifyOutputRequest or (Format   t) = or{oFormat  = unpack t}
modifyOutputRequest or (Scale    t) = error "Scale is not implemented" 
modifyOutputRequest or (Surround t) = or{oSurround = (preStr, postStr)} 
    where 
  (preStr:postStr:other) = stripSplitCommas t

