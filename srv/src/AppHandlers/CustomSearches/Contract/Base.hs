{-|

This module enables 'extraContractFields' to be used in in TH splices
of parent module.

-}

module AppHandlers.CustomSearches.Contract.Base
    ( extraContractFields
    )

where

import Data.Model
import Carma.Model.Contract


-- | Extra fields included in contract search results in addition to
-- contract identifier fields.
extraContractFields :: [FA Contract]
extraContractFields = [ FA make
                      , FA model
                      ]
