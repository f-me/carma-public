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
