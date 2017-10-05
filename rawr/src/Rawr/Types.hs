module Rawr.Types ( TestTree(..)
                  , TestState(..)
                  , TestPath
                  , rawr
                  ) where

import Prelude hiding (pow)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Functor.Identity
import qualified System.IO as SIO
import qualified Control.Parallel.Strategies as CPS
import qualified Data.List as DL
import qualified System.Console.ANSI as SCA
import qualified Text.PrettyPrint.Leijen as P 
          
data TestTree = TestTree String [TestTree]
              | TestCase String (IO TestState)
              
data TestState = Pass
               | Fail P.Doc
               deriving (Show)

rawr = Fail . P.text 

type TestPath = [String]
