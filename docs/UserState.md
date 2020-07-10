User states are used to track when operators were busy doing work or idle
waiting for a call. History of user state changes is stored in the
[`UserState`](../database/baseline/1-tables/4-UserState.sql)
table.

There is a [bunch of SQL functions](../database/baseline/2-functions/2-kpis.sql)
that use this table to calculate various KPIs.

User state change can be triggered by a number of events (grep for `logCRUD*`,
`updateUserState`:
  - Directly from createHandler & updateHandler.
    - User state can be changed when something is created or updated via HTTP API.
  - Triggers.hs
    - When a call is created, a new action is also created and linked to it. So we need to trigger user state change on action creation.
    - Transferring an action causes new action creation. Again, explicit user state update is required.
  - Backoffice DSL
    - `closePrevious`: closing related actions.
    - `openAction`: updating an action with `openTime`.
  - Avaya
    - `switchToNA`, `switchToReady`, `forceBusyUserToServiceBreak`.


Seems like architecture gone wild here. Would be better to have a single
place in the processing pipeline where new user states are created.

Current UserState is also planted into user's `Usermeta.stuff` JSON field. Maybe
this can be replaced with SQL view.

The code in [`Utils.Events`](../srv/src/Utils/Events.hs) is quite complicated
and calls for refactoring.


### Unanswered questions:
  - what is a `delayedState`?
