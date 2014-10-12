CREATE OR REPLACE FUNCTION get_KPI_timeinstate(u_id integer[], range tstzrange)
 RETURNS TABLE (
 userid         integer,
 "LoggedOut"    interval,
 "Ready"        interval,
 "Rest"         interval,
 "Busy"         interval,
 "Dinner"       interval,
 "ServiceBreak" interval,
 "totalRest"    interval,
 "totalLoggedIn"   interval) AS
$func$
BEGIN
  RETURN QUERY
SELECT *,
   coalesce(ct."Rest",         interval '0') +
   coalesce(ct."Dinner",       interval '0') +
   coalesce(ct."ServiceBreak", interval '0') as totalRest,

   coalesce(ct."Rest",         interval '0') +
   coalesce(ct."Dinner",       interval '0') +
   coalesce(ct."ServiceBreak", interval '0') +
   coalesce(ct."Busy",         interval '0') +
   coalesce(ct."Ready",        interval '0') as totalLoggedIn
   FROM
crosstab( $$
   SELECT us.userid, us.state,
   sum( upper('$$||range||$$' * us.range)
      - lower('$$||range||$$' * us.range)) as "timeInState"
   FROM "UserState" us
   WHERE us.userid = any('$$ || u_id::text || $$')
   AND '$$|| range ||$$' && us.range
  GROUP BY us.userid, us.state
  ORDER BY 1,2
$$,
$$
SELECT unnest (array['LoggedOut',
                     'Ready',
                     'Rest',
                     'Busy',
                     'Dinner',
                     'ServiceBreak'])
$$) AS ct(userid int,
          "LoggedOut" interval,
          "Ready" interval,
          "Rest" interval,
          "Busy" interval,
          "Dinner" interval,
          "ServiceBreak" interval);
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_calls(u_id integer [],
    fromTime timestamptz,
    toTime   timestamptz)
 RETURNS TABLE (
 userid   integer,
 callType text,
 avgTime  interval,
 amount   bigint
) AS
$func$
BEGIN
  RETURN QUERY
  --"newCase" -"Кейс/Создание нового кейса"
  --"processingCase" - "Кейс/Обработка кейса"(Вторичное обращение)
  --"info" - все прочие звонки (информационные звонки)
SELECT
  calltbl.calltaker,
  (CASE
    WHEN calltbl.calltype NOT IN ('newCase', 'processingCase')
        OR calltbl.callType is null
    THEN 'info'
    ELSE calltbl.calltype
  END),
  SUM(calltbl.enddate-calltbl.calldate)/COUNT(id) AS avgtime,
  COUNT(calltbl.id) as amount
FROM calltbl
  WHERE calltbl.calltaker = any(u_id)
  AND calltbl.calldate > fromTime
  AND calltbl.calldate < toTime
GROUP BY calltaker, (CASE
  WHEN calltbl.calltype NOT IN ('newCase', 'processingCase')
      OR calltbl.callType is null
  THEN 'info'
  ELSE calltbl.calltype
END);
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_actions(u_id integer [],
   c_time timestamp with time zone,
   end_time timestamp with time zone)
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
  usermetatbl.id,
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
  LEFT JOIN usermetatbl ON actiontbl.assignedto = usermetatbl.id

WHERE usermetatbl.id = any(u_id)
  AND actiontbl.closetime IS NOT NULL
  AND actiontbl.opentime IS NOT NULL
  AND actiontbl.assigntime BETWEEN c_time AND end_time
  AND actiontbl.closetime IS NOT NULL
  AND actiontbl.assigntime IS NOT NULL
  AND actiontbl.type IN (3, 4, 6, 9, 10, 11, 1, 19, 20)
GROUP BY usermetatbl.id, (CASE
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

CREATE OR REPLACE FUNCTION get_KPI_controll_actions(u_id integer [],
    c_time timestamp with time zone,
    end_time timestamp with time zone)
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
actiontbl.assignedto,
COUNT(1) as assigned,
COUNT(COALESCE(actiontbl.closetime, NOW())>actiontbl.duetime OR NULL)
  as assigned_overdue,
COUNT(actiontbl.result) as closed,
COUNT((actiontbl.result IS NOT NULL AND
       COALESCE(actiontbl.closetime, NOW()) > actiontbl.duetime)
      OR NULL)
  as closed_overdue,
COUNT((actiontbl.result IS NULL) OR NULL) as unclosed,
COUNT((actiontbl.result IS NULL AND
       COALESCE(actiontbl.closetime, NOW()) > actiontbl.duetime)
      OR NULL)
  as unclosed_overdue
FROM actiontbl
WHERE actiontbl.type IN (3, 4, 6, 9, 10, 11)
AND assignedto = any (u_id)
AND actiontbl.assigntime IS NOT NULL
AND actiontbl.assigntime BETWEEN c_time AND end_time
GROUP BY assignedto;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_sumcalls(u_id integer [],
       c_time timestamp with time zone,
       end_time timestamp with time zone)
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
calltbl.calltaker,
SUM(coalesce(calltbl.enddate, NOW()) - calltbl.calldate) AS calltime,
COUNT(calltbl.id) as amount,
SUM(coalesce(calltbl.enddate, NOW()) - calltbl.calldate) / COUNT(calltbl.id)
  AS avgtime
FROM calltbl
WHERE calltbl.calltaker = any(u_id)
AND calltbl.calldate > c_time
AND calltbl.calldate < end_time
GROUP BY calltaker;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_sum_orderactions(u_id integer [],
       c_time timestamp with time zone,
       end_time timestamp with time zone)
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
actiontbl.assignedto,
COUNT(actiontbl.id) as amount,
sum(actiontbl.closetime - actiontbl.assigntime) / COUNT(actiontbl.id) as avgtime
FROM actiontbl
WHERE actiontbl.assignedto = any(u_id)
AND actiontbl.result IS NOT NULL
AND actiontbl.assigntime BETWEEN c_time AND end_time
AND actiontbl.type IN  (1, 19, 20)
GROUP BY actiontbl.assignedto;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_utilization(u_id integer [],
       c_time timestamp with time zone,
       end_time timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 utilization double precision)
 AS
$func$
BEGIN
  RETURN QUERY
SELECT t.userid,
       EXTRACT(EPOCH FROM "Busy") / EXTRACT(EPOCH FROM "totalLoggedIn")
       AS utilization
FROM get_KPI_timeinstate(u_id, tstzrange(c_time, end_time)) t;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_avg_actdo(u_id integer [],
       c_time timestamp with time zone,
       end_time timestamp with time zone)
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
AND a.assigntime IS NOT NULL
AND a.assigntime BETWEEN c_time AND end_time
GROUP BY a.assignedto;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_actions_relation(u_id integer [],
       c_time timestamp with time zone,
       end_time timestamp with time zone)
 RETURNS TABLE (
 userid integer,
 "actionsRelation" double precision
 ) AS
$func$
DECLARE allOperators int;
        actsPerOp double precision;
BEGIN

SELECT COUNT(1) INTO allOperators
    FROM actiontbl a
    WHERE a.type IN (3, 4, 6, 9, 10, 11)
    AND a.result IS NOT NULL
    AND a.opentime BETWEEN c_time AND end_time
    GROUP BY a.assignedto;

SELECT COUNT(1) / allOperators INTO actsPerOp
    FROM actiontbl a
    WHERE a.result IS NOT NULL
    AND a.type IN (3, 4, 6, 9, 10, 11)
    AND a.opentime BETWEEN c_time AND end_time;

RETURN QUERY
SELECT a.assignedto,
       COUNT(a.assignedto)::double precision / actsPerOp
       AS "actionsRelation"
    FROM actiontbl a
    WHERE a.result IS NOT NULL
    AND a.type IN (3, 4, 6, 9, 10, 11)
    AND a.assignedto = any(u_id)
    AND a.opentime BETWEEN c_time AND end_time
    GROUP BY a.assignedto;
END;
$func$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_KPI_time_relation(u_id integer [], c_time timestamp with time zone, end_time timestamp with time zone)
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
  AND opentime BETWEEN c_time AND end_time;

RETURN QUERY
SELECT assignedto,
       avgPerEmp / EXTRACT(EPOCH FROM AVG(closetime - opentime))
       AS "timeRelation"
FROM actiontbl a
WHERE a.closetime IS NOT NULL
AND a.type IN (3, 4, 6, 9, 10, 11)
AND a.assignedto = any(u_id)
AND a.opentime BETWEEN c_time AND end_time
GROUP BY a.assignedto;
END;
$func$
LANGUAGE plpgsql;

-------- group kpis -------------------------------------------------------------

CREATE OR REPLACE FUNCTION group_kpi_calls(
    fromTime timestamptz,
    toTime   timestamptz)
 RETURNS TABLE (
 callType text,
 callTime  interval,
 amount   bigint
) AS
$func$
BEGIN
  RETURN QUERY
  --"newCase" -"Кейс/Создание нового кейса"
  --"processingCase" - "Кейс/Обработка кейса"(Вторичное обращение)
  --"info" - все прочие звонки (информационные звонки)
SELECT
  (CASE
    WHEN c.calltype NOT IN ('newCase', 'processingCase') OR c.callType IS NULL
    THEN 'info'
    ELSE c.calltype
  END),
  SUM(enddate - calldate) AS callTime,
  COUNT(id) as amount
FROM calltbl c
  WHERE calldate > fromTime
    AND calldate < toTime
GROUP BY (CASE
    WHEN c.calltype NOT IN ('newCase', 'processingCase') OR c.callType IS NULL
    THEN 'info'
    ELSE c.calltype
  END);
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
    10, --'Услуга оказана'
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
BEGIN
  RETURN QUERY
SELECT
       SUM(EXTRACT(EPOCH FROM "Busy")) / SUM(EXTRACT(EPOCH FROM "totalLoggedIn"))
       AS utilization
FROM get_KPI_timeinstate(
       (SELECT array_agg(id) FROM usermetatbl where showKPI = 't'),
       tstzrange(c_time, e_time)) t;
END;
$func$
LANGUAGE plpgsql;
