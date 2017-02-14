{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppHandlers.DiagTree
    ( diagInfo
    , diagHistory
    , retryQuestion
    )

where

import           Data.Aeson as A
import           Database.PostgreSQL.Simple.SqlQQ

import           Snap
import           Snap.Snaplet.PostgresqlSimple
import           Snaplet.Auth.PGUsers (currentUserMetaId)

import           AppHandlers.Util
import           Application


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


diagHistory :: AppHandler ()
diagHistory = do
  caseId <- getParam "caseId"
  hist <- query [sql|
    select row_to_json(x) from
      (select
          h.id, to_char(h.ctime, 'YYYY-MM-DD HH24-MI') as "answerTime",
          u.login as "user", h.answerIx as "answerIx",
          s.header, s.body, s.answers
        from "DiagHistory" h join "DiagSlide" s on (h.slideId = s.id)
          join usermetatbl u on (h.userId = u.id)
        where h.caseId = ?
          and snapshotId is null
        order by h.ctime asc) x
    |] [caseId]
  writeJSON (map fromOnly hist :: [A.Value])


retryQuestion :: AppHandler ()
retryQuestion = do
  histId <- getParam "histId"
  Just userId <- currentUserMetaId
  hist <- execute [sql|
      with
        snapId(value) as (select nextval('"DiagHistory_snapshot_seq"')),
        snapshots(value) as (
          select array_to_json(array_agg(snapshots.json))
            from
              ((select json_array_elements(snapshots)
                  from "DiagHistory" where id = ?)
                union all
                select row_to_json(x.*)
                  from
                    (select
                        u.id as "userId", u.login as "userLogin",
                        snapId.value as "snapshotId",
                        now() as ctime
                      from usermetatbl u, snapId
                        where u.id = ?
                    ) x
              ) snapshots(json)
      )
      update "DiagHistory" h
        set
          answerIx   = case h.id when h1.id then null else h.answerIx end,
          snapshotId = case h.id when h1.id then null else snapId.value end,
          snapshots  = case h.id when h1.id then snapshots.value else h.snapshots end
        from "DiagHistory" h1, snapId, snapshots
        where h1.id = ?
          and h.caseId = h1.caseId
          and h.ctime >= h1.ctime
          and (h.snapshotId is null or h.id = h1.id)
    |] (histId, userId, histId)
  writeJSON ()
