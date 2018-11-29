-- | Helpers for building/defining Era Glonass types.
module Carma.EraGlonass.Types.Helpers
     ( constructorsBranchingSchemaProto
     ) where

import           Data.Swagger


-- | Prototype "Schema" for branching constructors
constructorsBranchingSchemaProto :: Schema
constructorsBranchingSchemaProto = mempty
  { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
  , _schemaMinProperties = Just 1
  , _schemaMaxProperties = Just 1
  }
