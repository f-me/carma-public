CREATE OR REPLACE FUNCTION get_KPI_userstates(u_id integer[],
       b timestamptz,
       e timestamptz)
RETURNS TABLE (
 userid         integer,
 state          "UserStateVal",
 timeInState    interval) AS
$func$
BEGIN
  RETURN QUERY
   -- определим последние даты смены статусов пользователей
    with max_dates as (
    select us_total.userid, max(us_total.ctime) as max_ctime
    from "UserState" us_total
    WHERE range is null
    group by us_total.userid
  )
  select
     us.userid
    ,us.state
    ,SUM (LEAST(upper(us.range), upper(ur.dates), now()) - GREATEST(us.ctime, lower(ur.dates))) as timeinstate
  from "UserState" us
  -- исключим все некорректные смены статусов, у которых нету даты завершения, при условии, что есть более поздние статусы
  inner join max_dates md on us.userid = md.userid and ((us.ctime < md.max_ctime and us.range is not null) or (us.ctime = md.max_ctime and us.range is null))
  cross join (select tstzrange(b, e) as dates) ur
  -- для последних статусов выберем только те записи, которые созданы до конца указанного нами диапазона
  WHERE ctime >= lower(ur.dates) and (us.ctime<= upper(ur.dates) /*or ( upper(us.range) is null and us.ctime <= upper (us.range))*/)
  and (u_id = '{}' OR us.userid = any(u_id))
  GROUP BY
     us.userid
    ,us.state;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_userstates_days(u_id integer[],
       b timestamptz,
       e timestamptz)
RETURNS TABLE (
 userid         integer,
 day            date,
 state          "UserStateVal",
 timeInState    interval) AS
$func$
DECLARE
  r tstzrange := tstzrange(b, e);
BEGIN
  RETURN QUERY
   SELECT us.userid, date(g) as sday, us.state,
   SUM(
    upper(
    tstzrange(g, g + '1day') *
    coalesce(us.range, tstzrange(us.ctime, now())))
    -
    lower(
    tstzrange(g, g + '1day') *
    coalesce(us.range, tstzrange(us.ctime, now())))
    ) as "timeInState"
   FROM "UserState" us
   JOIN generate_series(b, e, '1 day') g
   ON tstzrange(g, g + '1day') && coalesce(us.range, tstzrange(us.ctime, now()))
   WHERE us.userid = any(u_id)
  GROUP BY us.userid, date(g), us.state
  ORDER BY 1,2,3;
END;
$func$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_KPI_calls(u_id integer [],
    fromTime timestamptz,
    toTime   timestamptz)
 RETURNS TABLE (
 userid   integer,
 callType integer,
 avgTime  interval,
 amount   bigint
) AS
$func$
BEGIN
  RETURN QUERY
SELECT
  calltbl.calltaker,
  calltbl.calltype,
  SUM(calltbl.enddate-calltbl.calldate)/COUNT(id) AS avgtime,
  COUNT(calltbl.id) as amount
FROM calltbl
  WHERE calltbl.calltaker = any(u_id)
  AND calltbl.calldate > fromTime
  AND calltbl.calldate < toTime
GROUP BY calltaker, calltbl.callType;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_calls_days(u_id integer [],
    fromTime timestamptz,
    toTime   timestamptz)
 RETURNS TABLE (
 userid   integer,
 day      date,
 callType integer,
 avgTime  interval,
 amount   bigint
) AS
$func$
BEGIN
  RETURN QUERY
SELECT
  c.calltaker, date(g), c.calltype,
  SUM(c.enddate-c.calldate)/COUNT(id) AS avgtime,
  COUNT(c.id) as amount
FROM calltbl c
JOIN generate_series(fromTime, toTime, '1 day') g
ON tstzrange(g, g + '1day') @> c.calldate
  WHERE c.calltaker = any(u_id)
  AND c.calldate BETWEEN fromTime AND toTime
GROUP BY calltaker, date(g), c.calltype;

END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_actions(u_id integer [],
   fromTime timestamp with time zone,
   toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 actiontype text,
 avgtime interval,
 amount bigint
 ) AS
$func$
BEGIN
  RETURN QUERY
--ЗАКАЗ
  --"orderService" - "Заказ услуги"
  --"rushOrder" - "Заказ услуги (аврал)"
  --"tellMeMore" - "Заказ услуги (требуется дополнительная информация)"
  --"callMeMaybe" - "Заказ услуги через мобильное приложение"
--КОНТРОЛЬ
 --"control":
  --"tellClient" - "Сообщение клиенту о договорённости"
  --"checkStatus" - "Уточнить статус оказания услуги у партнёра"
  --"checkEndOfService" - "Уточнения после оказания услуги"
  --"cancelService" - "Отказ от услуги"
  --"carmakerApproval" - "Согласование с автопроизводителем"
  --"tellMakerDenied" - "Оповещение об отказе автопроизводителя"
SELECT
  um.id,

  CASE
    WHEN (a.type IN (3, 4, 6, 9, 10, 11))
      THEN 'control'
    WHEN (a.type = 1)
      THEN 'orderService'
    WHEN (a.type = 24)
      THEN 'rushOrder'
    WHEN (a.type = 19)
      THEN 'tellMeMore'
    WHEN (a.type = 20)
      THEN 'callMeMaybe'
    ELSE
      a.type::text
  END AS actiontype,

  sum(a.closetime - a.opentime) / COUNT(a.id) as avgtime,

  COUNT(a.id) as amount
FROM actiontbl a
  LEFT JOIN usermetatbl um ON a.assignedto = um.id

WHERE um.id = any(u_id)
  AND a.assigntime BETWEEN fromTime AND toTime
  AND a.closetime IS NOT NULL
  AND a.opentime  IS NOT NULL
  AND a.type IN (3, 4, 6, 9, 10, 11, 1, 24, 19, 20)
  AND a.result <> 32 -- Закрыто супервизором
  AND a.result is not null
GROUP BY um.id, (CASE
    WHEN (a.type IN (3, 4, 6, 9, 10, 11))
      THEN 'control'
    WHEN (a.type = 1)
      THEN 'orderService'
    WHEN (a.type = 24)
      THEN 'rushOrder'
    WHEN (a.type = 19)
      THEN 'tellMeMore'
    WHEN (a.type = 20)
      THEN 'callMeMaybe'
    ELSE
      a.type::text
  END);
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_actions_days(u_id integer [],
   fromTime timestamp with time zone,
   toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 day    date,
 actiontype text,
 avgtime interval,
 amount bigint
 ) AS
$func$
BEGIN
  RETURN QUERY
--ЗАКАЗ
  --"orderService" - "Заказ услуги"
  --"rushOrder" - "Заказ услуги (аврал)"
  --"tellMeMore" - "Заказ услуги (требуется дополнительная информация)"
  --"callMeMaybe" - "Заказ услуги через мобильное приложение"
--КОНТРОЛЬ
 --"control":
  --"tellClient" - "Сообщение клиенту о договорённости"
  --"checkStatus" - "Уточнить статус оказания услуги у партнёра"
  --"checkEndOfService" - "Уточнения после оказания услуги"
  --"cancelService" - "Отказ от услуги"
  --"carmakerApproval" - "Согласование с автопроизводителем"
  --"tellMakerDenied" - "Оповещение об отказе автопроизводителя"
SELECT
  um.id, date(g),

  CASE
    WHEN (a.type IN (3, 4, 6, 9, 10, 11))
      THEN 'control'
    WHEN (a.type = 1)
      THEN 'orderService'
    WHEN (a.type = 24)
      THEN 'rushOrder'
    WHEN (a.type = 19)
      THEN 'tellMeMore'
    WHEN (a.type = 20)
      THEN 'callMeMaybe'
    ELSE
      a.type::text
  END AS actiontype,

  sum(a.closetime - a.opentime) / COUNT(a.id) as avgtime,

  COUNT(a.id) as amount
FROM actiontbl a
  LEFT JOIN usermetatbl um ON a.assignedto = um.id

  JOIN generate_series(fromTime, toTime, '1 day') g
  ON tstzrange(g, g + '1day') @> a.assigntime

WHERE um.id = any(u_id)
  AND a.assigntime BETWEEN fromTime AND toTime
  AND a.closetime IS NOT NULL
  AND a.opentime  IS NOT NULL
  AND a.type IN (3, 4, 6, 9, 10, 11, 1, 24, 19, 20)
  AND a.result <> 32 -- Закрыто супервизором
  AND a.result is not null
GROUP BY um.id, g, (CASE
    WHEN (a.type IN (3, 4, 6, 9, 10, 11))
      THEN 'control'
    WHEN (a.type = 1)
      THEN 'orderService'
    WHEN (a.type = 24)
      THEN 'rushOrder'
    WHEN (a.type = 19)
      THEN 'tellMeMore'
    WHEN (a.type = 20)
      THEN 'callMeMaybe'
    ELSE
      a.type::text
  END);
END;
$func$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_KPI_controll_actions(u_id integer [],
    fromTime timestamp with time zone,
    toTime timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 assigned bigint,
 assigned_overdue bigint,
 closed bigint,
 closed_overdue bigint,
 unclosed bigint,
 unclosed_overdue bigint
 ) AS
$func$
BEGIN
  RETURN QUERY
--КОНТРОЛЬ
 --"controll":
  --"tellClient" - "Сообщение клиенту о договорённости"
  --"checkStatus" - "Уточнить статус оказания услуги у партнёра"
  --"checkEndOfService" - "Уточнения после оказания услуги"
  --"cancelService" - "Отказ от услуги"
  --"carmakerApproval" - "Согласование с автопроизводителем"
  --"tellMakerDenied" - "Оповещение об отказе автопроизводителя"
SELECT
a.assignedto,
COUNT(1) as assigned,
COUNT(COALESCE(a.closetime, NOW())>a.duetime OR NULL)
  as assigned_overdue,
COUNT(a.result) as closed,
COUNT((a.result IS NOT NULL AND
       COALESCE(a.closetime, NOW()) > a.duetime)
      OR NULL)
  as closed_overdue,
COUNT((a.result IS NULL) OR NULL) as unclosed,
COUNT((a.result IS NULL AND
       COALESCE(a.closetime, NOW()) > a.duetime)
      OR NULL)
  as unclosed_overdue
FROM actiontbl a

WHERE a.type IN (3, 4, 6, 9, 10, 11)
AND assignedto = any (u_id)
AND a.assigntime BETWEEN fromTime AND toTime
GROUP BY assignedto;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_controll_actions_days(u_id integer [],
    fromTime timestamp with time zone,
    toTime timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 day    date,
 assigned bigint,
 assigned_overdue bigint,
 closed bigint,
 closed_overdue bigint,
 unclosed bigint,
 unclosed_overdue bigint
 ) AS
$func$
BEGIN
  RETURN QUERY
--КОНТРОЛЬ
 --"controll":
  --"tellClient" - "Сообщение клиенту о договорённости"
  --"checkStatus" - "Уточнить статус оказания услуги у партнёра"
  --"checkEndOfService" - "Уточнения после оказания услуги"
  --"cancelService" - "Отказ от услуги"
  --"carmakerApproval" - "Согласование с автопроизводителем"
  --"tellMakerDenied" - "Оповещение об отказе автопроизводителя"
SELECT
a.assignedto, date(g),
COUNT(1) as assigned,
COUNT(COALESCE(a.closetime, NOW())>a.duetime OR NULL)
  as assigned_overdue,
COUNT(a.result) as closed,
COUNT((a.result IS NOT NULL AND
       COALESCE(a.closetime, NOW()) > a.duetime)
      OR NULL)
  as closed_overdue,
COUNT((a.result IS NULL) OR NULL) as unclosed,
COUNT((a.result IS NULL AND
       COALESCE(a.closetime, NOW()) > a.duetime)
      OR NULL)
  as unclosed_overdue
FROM actiontbl a
JOIN generate_series(fromTime, toTime, '1 day') g
  ON tstzrange(g, g + '1day') @> a.assigntime

WHERE a.type IN (3, 4, 6, 9, 10, 11)
AND assignedto = any (u_id)
AND a.assigntime BETWEEN fromTime AND toTime
GROUP BY assignedto, g;
END;
$func$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_KPI_sumcalls(u_id integer [],
       fromTime timestamp with time zone,
       toTime timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 calltime interval,
 "callAmount" bigint,
 "callAvgTime" interval
 ) AS
$func$
BEGIN
  RETURN QUERY
SELECT
c.calltaker,
SUM(coalesce(c.enddate, NOW()) - c.calldate) AS calltime,
COUNT(c.id) as amount,
SUM(coalesce(c.enddate, NOW()) - c.calldate) / COUNT(c.id)
  AS avgtime
FROM calltbl c

WHERE c.calltaker = any(u_id)
AND c.calldate BETWEEN fromTime AND toTime
GROUP BY calltaker;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_sumcalls_days(u_id integer [],
       fromTime timestamp with time zone,
       toTime timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 day    date,
 calltime interval,
 "callAmount" bigint,
 "callAvgTime" interval
 ) AS
$func$
BEGIN
  RETURN QUERY
SELECT
c.calltaker, date(g),
SUM(coalesce(c.enddate, NOW()) - c.calldate) AS calltime,
COUNT(c.id) as amount,
SUM(coalesce(c.enddate, NOW()) - c.calldate) / COUNT(c.id)
  AS avgtime
FROM calltbl c
JOIN generate_series(fromTime, toTime, '1 day') g
  ON tstzrange(g, g + '1day') @> c.calldate

WHERE c.calltaker = any(u_id)
AND c.calldate BETWEEN fromTime AND toTime
GROUP BY calltaker, date(g);
END;
$func$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_KPI_sum_orderactions(u_id integer [],
       fromTime timestamp with time zone,
       toTime timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 "actionsAmount" bigint,
 "actionsAvgtime" interval
 ) AS
$func$
BEGIN
  RETURN QUERY
--ЗАКАЗ
  --"orderService" - "Заказ услуги"
  --"tellMeMore" - "Заказ услуги (требуется дополнительная информация)"
  --"callMeMaybe" - "Заказ услуги через мобильное приложение"
SELECT
a.assignedto,
COUNT(a.id) as amount,
sum(a.closetime - a.assigntime) / COUNT(a.id) as avgtime
FROM actiontbl a

WHERE a.assignedto = any(u_id)
AND a.result IS NOT NULL
AND a.assigntime BETWEEN fromTime AND toTime
AND a.type IN  (1, 19, 20)
GROUP BY a.assignedto;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_sum_orderactions_days(u_id integer [],
       fromTime timestamp with time zone,
       toTime timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 day    date,
 "actionsAmount" bigint,
 "actionsAvgtime" interval
 ) AS
$func$
BEGIN
  RETURN QUERY
--ЗАКАЗ
  --"orderService" - "Заказ услуги"
  --"tellMeMore" - "Заказ услуги (требуется дополнительная информация)"
  --"callMeMaybe" - "Заказ услуги через мобильное приложение"
SELECT
a.assignedto, date(g),
COUNT(a.id) as amount,
sum(a.closetime - a.assigntime) / COUNT(a.id) as avgtime
FROM actiontbl a
JOIN generate_series(fromTime, toTime, '1 day') g
  ON tstzrange(g, g + '1day') @> a.assigntime

WHERE a.assignedto = any(u_id)
AND a.result IS NOT NULL
AND a.assigntime BETWEEN fromTime AND toTime
AND a.type IN  (1, 19, 20)
GROUP BY a.assignedto, date(g);
END;
$func$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_KPI_utilization(u_id integer [],
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 utilization double precision)
 AS
$func$
BEGIN
  RETURN QUERY

  WITH states AS (SELECT *  FROM get_KPI_userstates(u_id, fromTime, toTime)
  ), totalLoggedIn AS (
    SELECT us.userid, sum(us.timeInState) as timeInState
    FROM states us
    WHERE us.state in ('Ready', 'Busy', 'Rest', 'Dinner', 'ServiceBreak')
    GROUP BY us.userid
  ), busy AS (
    SELECT us.userid, us.timeInState
    FROM states us
    WHERE us.state = 'Busy'
  )

  SELECT b.userid,
         (EXTRACT(EPOCH FROM b.timeInState) / EXTRACT(EPOCH FROM t.timeInState))
         AS utilization
  FROM totalLoggedIn t
  JOIN busy b ON t.userid = b.userid;

END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_utilization_days(u_id integer [],
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 day    date,
 utilization double precision)
 AS
$func$
BEGIN
  RETURN QUERY

  WITH states as (
    SELECT *  FROM get_KPI_userstates_days(u_id, fromTime, toTime)
  ), totalLoggedIn AS (
    SELECT us.userid, us.day, sum(us.timeInState) as timeInState
    FROM states us
    WHERE us.state in ('Ready', 'Busy', 'Rest', 'Dinner', 'ServiceBreak')
    GROUP BY us.userid, us.day
  ), busy AS (
    SELECT us.userid, us.day, sum(us.timeInState) as timeInState
    FROM states us
    WHERE us.state = 'Busy'
    GROUP BY us.userid, us.day
  )

  SELECT b.userid, b.day,
         (EXTRACT(EPOCH FROM b.timeInState) / EXTRACT(EPOCH FROM t.timeInState))
         AS utilization
 FROM totalLoggedIn t
  JOIN busy b ON t.userid = b.userid AND t.day = b.day;

END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_avg_actdo(u_id integer [],
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 "avgActionOverdue" interval)
 AS
$func$
BEGIN
  RETURN QUERY

SELECT a.assignedto,
       SUM( coalesce(a.closetime, NOW())
          - coalesce(a.assigntime, a.closetime, NOW())
       ) / COUNT(a.id)
       AS "avgActionOverdue"
FROM actiontbl a

WHERE a.assignedto = any(u_id)
AND a.assigntime BETWEEN fromTime AND toTime
GROUP BY a.assignedto;

END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_avg_actdo_days(u_id integer [],
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 day    date,
 "avgActionOverdue" interval)
 AS
$func$
BEGIN
  RETURN QUERY

SELECT a.assignedto, date(g),
       SUM( coalesce(a.closetime, NOW())
          - coalesce(a.assigntime, a.closetime, NOW())
       ) / COUNT(a.id)
       AS "avgActionOverdue"

FROM actiontbl a
JOIN generate_series(fromTime, toTime, '1 day') g
  ON tstzrange(g, g + '1day') @> a.assigntime

WHERE a.assignedto = any(u_id)
AND a.assigntime BETWEEN fromTime AND toTime
GROUP BY a.assignedto, g;
END;

$func$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_KPI_actions_relation(u_id integer [],
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 "actionsRelation" double precision
 ) AS
$func$
BEGIN
RETURN QUERY

WITH acts AS (
  SELECT a.assignedto
  FROM actiontbl a
  WHERE a.type IN (3, 4, 6, 9, 10, 11)
    AND a.result IS NOT NULL
    AND a.opentime BETWEEN fromTime AND toTime
  ), allOperators AS(
  SELECT COUNT(1) as c
      FROM acts a
      GROUP BY a.assignedto
  ), actsPerOp AS (
  SELECT COUNT(1) / allOperators.c as c
    FROM acts, allOperators
    GROUP BY allOperators.c
  )

SELECT a.assignedto,
       COUNT(a.assignedto)::double precision / o.c
       AS "actionsRelation"
    FROM acts a, actsPerOp o
    WHERE a.assignedto = any(u_id)
    GROUP BY a.assignedto, o.c;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_actions_relation_days(u_id integer [],
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 day    date,
 "actionsRelation" double precision
 ) AS
$func$
BEGIN
RETURN QUERY

WITH acts AS (
  SELECT a.*, date(g) as day
  FROM actiontbl a
  JOIN generate_series(fromTime, toTime, '1 day') g
    ON tstzrange(g, g + '1day') @> a.opentime
  WHERE a.type IN (3, 4, 6, 9, 10, 11)
    AND a.result IS NOT NULL
    AND a.opentime BETWEEN fromTime AND toTime
  ), allOperators AS (
  SELECT COUNT(1) as c
    FROM acts a
    GROUP BY a.assignedto
  ), actsPerOp AS(
  SELECT COUNT(1) / allOperators.c as c
    FROM acts, allOperators
    GROUP BY allOperators.c
  )

SELECT a.assignedto, a.day,
       COUNT(a.assignedto)::double precision / o.c
       AS "actionsRelation"
    FROM acts a, actsPerOp o
    WHERE a.assignedto = any(u_id)
    GROUP BY a.assignedto, a.day, o.c;

END;
$func$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_KPI_time_relation(u_id integer [],
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 "timeRelation" double precision
 ) AS
$func$
DECLARE
  avgPerEmp double precision;
BEGIN

SELECT EXTRACT(EPOCH FROM AVG(closetime - opentime)) INTO avgPerEmp
  FROM actiontbl a
  WHERE closetime IS NOT NULL
  AND type IN (3, 4, 6, 9, 10, 11)
  AND opentime BETWEEN fromTime AND toTime;

RETURN QUERY
SELECT assignedto,
       avgPerEmp / EXTRACT(EPOCH FROM AVG(closetime - opentime))
       AS "timeRelation"
FROM actiontbl a
WHERE a.closetime IS NOT NULL
AND a.type IN (3, 4, 6, 9, 10, 11)
AND a.assignedto = any(u_id)
AND a.opentime BETWEEN fromTime AND toTime
GROUP BY a.assignedto;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_time_relation_days(u_id integer [],
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 day    date,
 "timeRelation" double precision
 ) AS
$func$
BEGIN
RETURN QUERY

WITH acts AS (
  SELECT a.*, date(g) as day
  FROM actiontbl a
  JOIN generate_series(fromTime, toTime, '1 day') g
    ON tstzrange(g, g + '1day') @> a.opentime
  WHERE closetime IS NOT NULL
    AND type IN (3, 4, 6, 9, 10, 11)
    AND opentime BETWEEN fromTime AND toTime
  ), avgPerEmp AS (
  SELECT EXTRACT(EPOCH FROM AVG(closetime - opentime)) as c
  FROM acts
  )

SELECT assignedto, a.day,
       avgPerEmp.c / EXTRACT(EPOCH FROM AVG(closetime - opentime))
       AS "timeRelation"
FROM acts a, avgPerEmp
WHERE a.assignedto = any(u_id)
GROUP BY a.assignedto, a.day, avgPerEmp.c;

END;
$func$
LANGUAGE plpgsql;


-------- group kpis -------------------------------------------------------------

CREATE OR REPLACE FUNCTION group_kpi_calls(
    fromTime timestamptz,
    toTime   timestamptz)
 RETURNS TABLE (
 callType integer,
 callTime  interval,
 amount   bigint
) AS
$func$
BEGIN
  RETURN QUERY
SELECT c.callType,
  SUM(enddate - calldate) AS callTime,
  COUNT(id) as amount
FROM calltbl c
  WHERE calldate > fromTime
    AND calldate < toTime
GROUP BY c.calltype;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_sumcalls(c_time timestamp with time zone,
                                              e_time timestamp with time zone)
 RETURNS TABLE (
  "callAmount" bigint,
  calltime interval,
  "callAvgTime" interval
 ) AS
$func$
BEGIN
  RETURN QUERY
SELECT
COUNT(calltbl.id) as "callAmount",
SUM(coalesce(calltbl.enddate, NOW()) - calltbl.calldate) as "calltime",
SUM(coalesce(calltbl.enddate, NOW()) - calltbl.calldate) / COUNT(calltbl.id)
AS "callAvgTime"
FROM calltbl
WHERE calltbl.calldate IS NOT NULL
AND calltbl.calldate BETWEEN c_time AND e_time;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_actions(
   c_time timestamp with time zone,
   end_time timestamp with time zone)
 RETURNS TABLE (
 actiontype text,
 avgtime interval,
 amount bigint
 ) AS
$func$
BEGIN
  RETURN QUERY
--ЗАКАЗ
  --"orderService" - "Заказ услуги"
  --"tellMeMore" - "Заказ услуги (требуется дополнительная информация)"
  --"callMeMaybe" - "Заказ услуги через мобильное приложение"
--КОНТРОЛЬ
 --"control":
  --"tellClient" - "Сообщение клиенту о договорённости"
  --"checkStatus" - "Уточнить статус оказания услуги у партнёра"
  --"checkEndOfService" - "Уточнения после оказания услуги"
  --"cancelService" - "Отказ от услуги"
  --"carmakerApproval" - "Согласование с автопроизводителем"
  --"tellMakerDenied" - "Оповещение об отказе автопроизводителя"
SELECT
  CASE
  WHEN (actiontbl.type IN (3, 4, 6, 9, 10, 11))
  THEN 'control'
  WHEN (actiontbl.type = 1)
  THEN 'orderService'
  WHEN (actiontbl.type = 19)
  THEN 'tellMeMore'
  WHEN (actiontbl.type = 20)
  THEN 'callMeMaybe'
  ELSE actiontbl.type::text
  END AS actiontype,

  sum(actiontbl.closetime-actiontbl.opentime)/COUNT(actiontbl.id) as avgtime,

  COUNT(actiontbl.id) as amount

FROM actiontbl
WHERE actiontbl.closetime IS NOT NULL
  AND actiontbl.opentime IS NOT NULL
  AND actiontbl.assigntime BETWEEN c_time AND end_time
  AND actiontbl.closetime IS NOT NULL
  AND actiontbl.assigntime IS NOT NULL
  AND actiontbl.type IN (3, 4, 6, 9, 10, 11, 1, 19, 20)
GROUP BY (CASE
    WHEN (actiontbl.type IN (3, 4, 6, 9, 10, 11))
    THEN 'control'
    WHEN (actiontbl.type = 1)
    THEN 'orderService'
    WHEN (actiontbl.type = 19)
    THEN 'tellMeMore'
    WHEN (actiontbl.type = 20)
    THEN 'callMeMaybe'
    ELSE actiontbl.type::text
  END);
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_all_actions(
       c_time timestamp with time zone,
       e_time timestamp with time zone)
 RETURNS TABLE (
 totalActionsAmount  bigint,
 totalActionsAvgTime interval
 ) AS
$func$
BEGIN
  RETURN QUERY
SELECT
  COUNT(id) as totalActionsAmount,
  sum(closetime - opentime) / COUNT(id) as totalActionsAvgTime
FROM actiontbl
WHERE closetime IS NOT NULL
AND assigntime BETWEEN c_time AND e_time
AND closetime  IS NOT NULL
AND opentime   IS NOT NULL;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_services_done(
       c_time timestamp with time zone,
       e_time timestamp with time zone)
 RETURNS TABLE (
 doneServices bigint
 ) AS
$func$
BEGIN
  RETURN QUERY
  SELECT COUNT(CASE WHEN (servicetbl.status IN (
    19, --'Услуга оказана'
    15, --"Услуга заказана"
    16, --"Оказание услуги задерживается"
    17, --"Услуга оказывается"
    20 --'Услуга закрыта'
  )) THEN servicetbl.id
  END) as done_services
FROM servicetbl
WHERE servicetbl.createtime IS NOT NULL
AND servicetbl.createtime BETWEEN c_time AND e_time;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_allservices(
       c_time timestamp with time zone,
       e_time timestamp with time zone)
 RETURNS TABLE (
 allServicesAmount bigint,
 allServicesAvgAwaitingTime interval,
 allServicesAvgRenderTime interval
 ) AS
$func$
BEGIN
  RETURN QUERY
  SELECT
    COUNT((s.status != 9) OR NULL) as amount, --"Ошибка оператора"

    AVG(
      CASE
        WHEN (s.times_factservicestart IS NOT NULL
         AND c.calldate IS NOT NULL)
        THEN s.times_factservicestart - c.calldate END
        ) as avgAwaitingTime,

    AVG(s.times_factserviceend - s.times_factservicestart) as avgRenderTime

  FROM servicetbl s INNER JOIN casetbl c
  ON s.parentid = c.id
  WHERE s.createtime IS NOT NULL
    AND s.createtime BETWEEN c_time AND e_time;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_controll_actions(
    c_time timestamp with time zone,
    end_time timestamp with time zone)
 RETURNS TABLE (
 assigned bigint,
 assigned_overdue bigint,
 closed bigint,
 closed_overdue bigint,
 unclosed bigint,
 unclosed_overdue bigint
 ) AS
$func$
BEGIN
  RETURN QUERY
--КОНТРОЛЬ
 --"controll":
  --"tellClient" - "Сообщение клиенту о договорённости"
  --"checkStatus" - "Уточнить статус оказания услуги у партнёра"
  --"checkEndOfService" - "Уточнения после оказания услуги"
  --"cancelService" - "Отказ от услуги"
  --"carmakerApproval" - "Согласование с автопроизводителем"
  --"tellMakerDenied" - "Оповещение об отказе автопроизводителя"
SELECT
COUNT(1) as assigned,
COUNT(COALESCE(closetime, NOW()) > duetime OR NULL)
  as assigned_overdue,
COUNT(result) as closed,
COUNT((result IS NOT NULL AND
       COALESCE(closetime, NOW()) > duetime)
      OR NULL)
  as closed_overdue,
COUNT((result IS NULL) OR NULL) as unclosed,
COUNT((result IS NULL AND
       COALESCE(closetime, NOW()) > duetime)
      OR NULL)
  as unclosed_overdue
FROM actiontbl
WHERE type IN (3, 4, 6, 9, 10, 11)
AND duetime IS NOT NULL
AND assigntime BETWEEN c_time AND end_time;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_utilization(
       c_time timestamp with time zone,
       e_time timestamp with time zone)
 RETURNS TABLE (
 utilization double precision)
 AS
$func$
DECLARE
  inSystem interval;
  busy     interval;
BEGIN

  SELECT sum(us.timeInState) INTO inSystem
    FROM get_KPI_userstates('{}', c_time, e_time) us
    WHERE us.state in ('Ready', 'Busy', 'Rest', 'Dinner', 'ServiceBreak');

  SELECT SUM(us.timeInState) INTO busy
    FROM get_KPI_userstates('{}', c_time, e_time) us
    WHERE us.state = 'Busy';

  RETURN QUERY
  SELECT (EXTRACT(EPOCH FROM busy) / EXTRACT(EPOCH FROM inSystem))
         AS utilization;

END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION absinterval(interval) RETURNS interval
    IMMUTABLE LANGUAGE sql AS 'SELECT greatest($1,-$1)';

CREATE OR REPLACE FUNCTION group_kpi_avgSrvProcessing(
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 avgSrvProcessing interval)
 AS
$func$
BEGIN

  RETURN QUERY
  SELECT avg(a.closetime - e.ctime)
  FROM servicetbl s
    JOIN "Event" e
    ON s.id = e.modelId AND e.modelName IN (
       SELECT value FROM "CtrModel" m JOIN "ServiceType" t
         on m.id = t.model)
    JOIN actiontbl a
    ON a.serviceid = s.id
    WHERE s.createTime BETWEEN fromTime AND toTime
      AND a.result IN (2, 1)
      AND patch->>'status' = '1'
      AND absinterval(s.times_expectedServiceStart - e.ctime) <= '1:0:0';
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_avgSrvFinish(
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 avgSrvFinish interval)
 AS
$func$
BEGIN

RETURN QUERY
SELECT avg(s.times_factServiceEnd - s.times_factServiceStart)
FROM servicetbl s
WHERE s.createTime BETWEEN fromTime AND toTime
  AND s.times_factServiceStart IS NOT NULL
  AND s.times_factServiceEnd   IS NOT NULL;

END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_satisfiedClients(
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 satisfiedClients bigint)
 AS
$func$
BEGIN

RETURN QUERY

SELECT CASE
  WHEN (count(1) > 0) THEN (count(clientSatisfied = 1 or null) / count(1))
  ELSE 0
  END
FROM servicetbl
WHERE createTime BETWEEN fromTime AND toTime;

END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_claims(
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 claimsCount bigint)
 AS
$func$
BEGIN

RETURN QUERY

SELECT count(1)
FROM servicetbl s
JOIN actiontbl a
ON s.id = a.serviceid
WHERE s.createTime BETWEEN fromTime AND toTime
  AND a.type = 18; -- претензия;

END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION group_kpi_towStartAvg(
       fromTime timestamp with time zone,
       toTime   timestamp with time zone)
 RETURNS TABLE (
 towStartAvgTime interval)
 AS
$func$
BEGIN

RETURN QUERY

WITH
 services AS (
  SELECT type, id, parentid, times_factServiceStart, times_expectedDispatch,
         contractor_partner, suburbanmilage
    FROM techtbl
  UNION ALL
  SELECT type, id, parentid, times_factServiceStart, times_expectedDispatch,
         contractor_partner, suburbanmilage
    FROM towagetbl)
SELECT
  avg(s.times_factServiceStart - s.times_expectedDispatch)
FROM casetbl c, services s
WHERE s.parentid = c.id
AND s.times_factServiceStart > s.times_expectedDispatch
AND coalesce(s.suburbanmilage, '0') = '0'
AND c.calldate BETWEEN fromTime AND toTime;

END;
$func$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_KPI_attachments(
       fromTime timestamptz,
       toTime   timestamptz)
 RETURNS TABLE (
 cases_amount bigint,
 files_attached bigint
 ) AS
$func$
BEGIN
  RETURN QUERY
  WITH case_files AS (
    SELECT id,
           split_part(regexp_split_to_table(files, ','),
                      'Attachment:',
                      2)::integer as attachment
  FROM casetbl
  WHERE calldate BETWEEN fromTime AND toTime
    AND files is not null and files != '')

SELECT COUNT(DISTINCT c.id) as cases_amount
     , COUNT(c.id) as files_attached
  FROM case_files c
  JOIN attachmenttbl a
  ON c.attachment = a.id
WHERE a.filename = 'з-н.pdf'
  AND a.ctime BETWEEN fromTime AND toTime;
END;
$func$
LANGUAGE plpgsql;

