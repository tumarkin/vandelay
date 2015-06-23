-- import App.Vandelay.Template.Types
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.RWS
import Data.List
import Data.Maybe
import Data.Monoid

-- Either IO Monad 
type EIO a = EitherT a IO


type MakeMonad = RWST Int String () (EIO String)


m :: MakeMonad Int
m = return (+) `ap` lift x `ap` ask 

x :: EIO String Int
x = return 5
