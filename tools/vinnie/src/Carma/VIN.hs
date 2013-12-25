module Carma.VIN
    ( getOptions
    , throwError
    )

where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Error as E
import           Control.Monad.Trans.Reader

import           Carma.VIN.Base


getOptions :: (Options -> a) -> Import a
getOptions proj = lift $ lift $ asks proj


throwError :: ImportError -> Import a
throwError err = lift $ E.throwError err
