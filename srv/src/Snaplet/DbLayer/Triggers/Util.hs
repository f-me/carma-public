{-|

Helpers for carma-models dictionaries used in legacy CRUD triggers.

-}

module Snaplet.DbLayer.Triggers.Util
    ( getCommentLabel
    , getCRRLabel
    )

where

import qualified Data.ByteString.Char8 as B
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           Snap.Snaplet.PostgresqlSimple ((:.)(..), Only(..))

import           Data.Model as Model
import           Data.Model.Sql
import qualified Carma.Model.Case.Type as Case
import qualified Carma.Model.ClientRefusalReason as CRR
import qualified Carma.Model.Service as Service
import qualified Carma.Model.Diagnostics.Wazzup as Wazzup

import           Snaplet.DbLayer.Triggers.Dsl
import           Snaplet.DbLayer.Triggers.Types
import           Snaplet.DbLayer.Util


-- | Fetch label of @comment@ field of a case.
getCommentLabel caseId = do
  c <- get caseId $ T.encodeUtf8 $ fieldName Case.comment
  case (B.readInt c) of
    Just (c', _) ->
        do
          [Only l :. ()] <- liftDb $ selectDb $
                            Wazzup.label :. Wazzup.ident `eq` Ident c'
          return l
    Nothing -> return $ T.decodeUtf8 c


-- | Fetch label of @clientCancelReason@ field of a service.
getCRRLabel caseId = do
  c <- get caseId $ T.encodeUtf8 $ fieldName Service.clientCancelReason
  case (B.readInt c) of
    Just (c', _) ->
        do
          [Only l :. ()] <- liftDb $ selectDb $
                            CRR.label :. CRR.ident `eq` Ident c'
          return l
    Nothing -> return $ T.decodeUtf8 c
