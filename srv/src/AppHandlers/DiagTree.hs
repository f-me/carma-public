{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppHandlers.DiagTree
    ( diagInfo
    , diagHistory
    , retryQuestion

    , MoveOrCopyDiagSlide (..)
    , moveOrCopyDiagSlide
    )

where

import           Data.Aeson as A
import           Database.Persist
import           Database.PostgreSQL.Simple.SqlQQ

import           Control.Monad.IO.Class (liftIO)

import           Snap
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.PostgresqlSimple
import           Snaplet.Auth.PGUsers (currentUserMetaId)

import           Carma.Model.DiagSlide.Persistent
import           AppHandlers.Util
import           Application


-- | Check if idag is possible or has started already
diagInfo :: AppHandler ()
diagInfo = do
  caseId <- getParam "caseId"
  [Only res] <- query [sql|
    with slides as
        (select
            h.ctime, s.isRoot,
            (s.answers->h.answerIx->>'isFinal')::bool as isFinal
          from "DiagHistory" h join "DiagSlide" s on (h.slideId = s.id)
          where caseId = ?
        )
      select row_to_json(x) from
        (select
          (select diagTree
            from "SubProgram" s join casetbl c on (s.id = c.subprogram)
            where c.id = ?) as root,
          (select ctime from slides where isRoot limit 1) as started,
          (select ctime from slides where isFinal limit 1) as ended
        ) x
    |] [caseId, caseId]
  writeJSON (res :: A.Value)


-- | Join history with slides
diagHistory :: AppHandler ()
diagHistory = do
  caseId <- getParam "caseId"
  hist <- query [sql|
    select row_to_json(x) from
      (select
          h.id, to_char(h.answerTime, 'YYYY-MM-DD HH24-MI') as "answerTime",
          u.login as "answeredBy", h.answerIx as "answerIx",
          h.deprecatedBy as "deprecatedBy",
          s.header, s.body, s.answers, s.resources, s.actions
        from "DiagHistory" h
          join "DiagSlide" s on (h.slideId = s.id)
          left outer join usermetatbl u on (h.answeredBy = u.id)
        where h.caseId = ?
        order by h.ctime asc) x
    |] [caseId]
  writeJSON (map fromOnly hist :: [A.Value])


retryQuestion :: AppHandler ()
retryQuestion = do
  histId <- getParam "histId"
  Just userId <- currentUserMetaId
  _ <- execute [sql|
      with newQ(id) as
        (insert into "DiagHistory" (caseId, slideId, createdBy)
          select caseId, slideId, ?::int
            from "DiagHistory"
            where id = ?
              and deprecatedBy is null
          returning "DiagHistory".id)
        update "DiagHistory" h
          set deprecatedBy = newQ.id
          from "DiagHistory" h1, newQ
          where h1.id = ?
            and h.caseId = h1.caseId
            and h.id <> newQ.id
            and h.ctime >= h1.ctime
            and h.deprecatedBy is null
    |] (userId, histId, histId)
  writeJSON ()


data MoveOrCopyDiagSlide = MoveDiagSlide | CopyDiagSlide

moveOrCopyDiagSlide :: MoveOrCopyDiagSlide -> AppHandler ()
moveOrCopyDiagSlide _ = do
  (res :: [Entity DiagSlide]) <-
    with db2 $ runPersist $
      selectList [] [LimitTo 10]

  liftIO $ print res
  writeJSON ()
