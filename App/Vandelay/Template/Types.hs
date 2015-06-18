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
    = Configuration { datafile      = datafile ca `mappend` datafile cb
                    , desiredModels = desiredModels   ca `mappend` desiredModels   cb
                    , texfile       = texfile  ca `mappend` texfile  cb
                    }





-- Table Commands

data TableCommand = Latex    Text
                  | Template Text
                  | Data     OutputRequest
                  deriving (Show)

data DataCommand = StatLine Int
                 | Name     Text
                 | Code     Text
                 | Index    Int
                 | Format   Text
                 | Scale    Text
                 | Surround Text
                 deriving (Show, Ord, Eq)









createOutputRequest :: OutputRequest -- Last complete output request, used for seeding statlines
                    -> [DataCommand] 
                    -> OutputRequest
createOutputRequest lcor dcs = 
  snd . foldl modifyOutputRequest (lcor, defaultOutputRequest) $ dcs 

modifyOutputRequest :: (OutputRequest, OutputRequest) -- (Last output request, Current output request)
                    -> DataCommand   
                    -> (OutputRequest, OutputRequest) -- (Last outputrequest, Resulting output request)
modifyOutputRequest (lor, or) (StatLine i) = (lor, lor{oName    = "", oItemIdx = i})
modifyOutputRequest (lor, or) (Name     t) = (lor,  or{oName    = unpack t})
modifyOutputRequest (lor, or) (Code     t) = (lor,  or{oCoeffs  = stripSplitCommas t})
modifyOutputRequest (lor, or) (Index    i) = (lor,  or{oItemIdx = i})
modifyOutputRequest (lor, or) (Format   t) = (lor,  or{oFormat  = unpack t})
modifyOutputRequest (lor, or) (Scale    t) = error "Scale is not implemented" 
modifyOutputRequest (lor, or) (Surround t) = (lor,  or{oSurround = (preStr, postStr)})
    where 
  (preStr:postStr:other) = stripSplitCommas t

