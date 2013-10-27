
module AppHandlers.UserAchievements where

import Data.Int
import Data.Text (Text)

import Snap.Snaplet.Auth hiding (session)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Application


userAchievements :: AuthUser -> AppHandler [Text]
userAchievements u = do
  res  <- withPG pg_search $ \c -> query c [sql|
    with
      calls  as (
        select count(*) as res from calltbl
          where callTaker = ?
            and calldate > now() - interval '20 days'),
      orders as (
        select count(*) as res from actiontbl
          where closed and assignedTo = ?
            and closeTime > now() - interval '20 days'
            and name = 'orderService'),
      actions as (
        select count(*) as res from actiontbl
          where closed and assignedTo = ?
            and closeTime > now() - interval '20 days'
            and dueTime <= closeTime)
      select calls.res, orders.res, actions.res
        from calls, orders, actions
    |] (userLogin u, userLogin u, userLogin u)

  case (res :: [[Int64]]) of
    [[calls,orders,actions]] -> return $ concat
      [if calls < 100 then []
        else ["Принято больше ста звонков за последние 20 дней"]
      ,if orders < 100 then []
        else ["Заказано больше ста услуг за последние 20 дней"]
      ,if actions < 100 then []
        else ["Больше ста действий выполнено вовремя за последние 20 дней"]
      ]
    _ -> error "in userAchievements"
