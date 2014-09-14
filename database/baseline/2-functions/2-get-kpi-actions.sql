--SELECT * FROM get_KPI_actions('{635, 278, 291}', '2014-06-07'::timestamp with time zone, '2014-07-29'::timestamp with time zone)
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
  WHEN (actiontbl.type IN
        (3,
         4,
         6,
         9,
         10,
         11))
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
  AND actiontbl.type IN
        (3,
         4,
         6,
         9,
         10,
         11,
         1,
         19,
         20)
GROUP BY usermetatbl.id, (CASE
    WHEN (actiontbl.type IN
          (3,
           4,
           6,
           9,
           10,
           11))
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
