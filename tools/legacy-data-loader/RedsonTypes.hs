{-# LANGUAGE OverloadedStrings #-}

module RedsonTypes where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M


-- {{{BEGIN copy-paste from Snap.Snaplet.Redson.Snapless.Metamodel
type ModelName = B.ByteString
type FieldName = B.ByteString
type FieldValue = B.ByteString

-- | Name of indexed field and collation flag.
type FieldIndex = (FieldName, Bool)

-- | List of field key-value pairs.
-- Suitable for using with 'Database.Redis.hmset'.
type Commit = M.Map FieldName FieldValue
-- END}}} copy-paste from Snap.Snaplet.Redson.Snapless.Metamodel


-- {{{BEGIN copy-paste Snap.Snaplet.Redson.Snapless.CRUD
type InstanceId = B.ByteString

-- | Build Redis key given model name and instance id
instanceKey :: ModelName -> InstanceId -> B.ByteString
instanceKey model id = B.concat [model, ":", id]
-- END}}} copy-paste Snap.Snaplet.Redson.Snapless.CRUD
