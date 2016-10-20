drop view if exists report_actions;
create view report_actions as
 SELECT c.id::text || 
        CASE
            WHEN serv.id IS NULL THEN ''::text
            WHEN first_value(serv.id) OVER (PARTITION BY c.id ORDER BY COALESCE(serv.id::bigint, 10000000000::bigint)) <> first_value(serv.id) OVER (PARTITION BY c.id ORDER BY COALESCE(serv.id, (-1)) DESC) THEN '/'::text || dense_rank() OVER (PARTITION BY c.id ORDER BY serv.id)::text
            ELSE ''::text
        END AS "Номер кейса/услуги", 
    serv.id AS "Номер услуги", 
    st.label AS "Тип обращения", 
    row_number() OVER (PARTITION BY c.id, serv.id ORDER BY act.id) AS "Действие в услуге", 
    act_t.label AS "Тип действия", 
    timezone('Europe/Moscow'::text, act.ctime) AS "Дата создания действия", 
    timezone('Europe/Moscow'::text, act.assigntime) AS "Время распределения", 
    timezone('Europe/Moscow'::text, act.duetime) AS "Ожидаемое время выполнения", 
    timezone('Europe/Moscow'::text, act.closetime) AS "Фактическое время выполнения", 
    um.realname AS "ФИО оператора", 
    ar.label AS "Результат", 
    act.comment AS "Комментарий на действии", 
    timezone('Europe/Moscow'::text, c.calldate) AS params_case_createtime, 
    timezone('Europe/Moscow'::text, serv.createtime) AS params_service_createtime, 
    timezone('Europe/Moscow'::text, act.ctime) AS params_action_createtime, 
    COALESCE(c.id::bigint, 10000000000::bigint) AS sort_case_id, 
    COALESCE(serv.id::bigint, 10000000000::bigint) AS sort_service_id, 
    act.id AS sort_act_id, 
    COALESCE(c.calldate::date, serv.createtime::date, act.ctime::date, '3000-01-01'::date) AS sort_date
   FROM actiontbl act
   LEFT JOIN servicetbl serv ON act.serviceid = serv.id
   LEFT JOIN casetbl c ON act.caseid = c.id
   LEFT JOIN "ActionType" act_t ON act.type = act_t.id
   LEFT JOIN usermetatbl um ON act.assignedto = um.id
   LEFT JOIN "ActionResult" ar ON act.result = ar.id
   LEFT JOIN "ServiceType" st ON serv.type = st.id;

grant select on report_actions to reportgen;

