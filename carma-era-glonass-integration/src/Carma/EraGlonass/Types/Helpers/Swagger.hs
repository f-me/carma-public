module Carma.EraGlonass.Types.Helpers.Swagger
     ( constructorsBranchingSchemaProto
     ) where

import           Data.Swagger


-- | Prototype "Schema" for branching constructors
constructorsBranchingSchemaProto :: Schema
constructorsBranchingSchemaProto = mempty
  { _schemaParamSchema = mempty { _paramSchemaType = Just SwaggerObject }
  , _schemaMinProperties = Just 1
  , _schemaMaxProperties = Just 1
  }
