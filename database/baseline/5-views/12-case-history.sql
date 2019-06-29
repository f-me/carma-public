BEGIN;

DROP VIEW IF EXISTS "CaseHistory";

CREATE VIEW "CaseHistory" AS
  SELECT caseId
       , datetime
       , usermetatbl.realName || ' (' || usermetatbl.login || ')' AS who
       , ROW_TO_JSON AS json

  FROM (

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
        SELECT
            'action' as "type",
            actiontbl.caseId AS caseId,
            actiontbl.closeTime AS datetime,
            actiontbl.assignedTo AS userId,
            "ActionType".label AS actionType,
            "ActionResult".label AS actionResult,
            actiontbl.comment AS actionComment,
            s.id AS serviceId,
            "ServiceType".label AS serviceLabel,
            (select (e.patch->'tasks')::json from "Event" e
                where e.modelid = actiontbl.serviceid
                  and e.modelname = 'AverageCommissioner'
                  and (e.patch->'tasks')::json is not null
                  and e.ctime <= actiontbl.closetime
                  and e.ctime >= s.createtime
                  and actiontbl.type = 1
                  and (actiontbl.result = 1 or actiontbl.result = 2)
                order by e.ctime desc
                limit 1
            ) as tasks
        FROM "ActionResult",
            "ActionType",
            actiontbl
        LEFT OUTER JOIN servicetbl s ON s.id = actiontbl.serviceId
        LEFT JOIN "ServiceType" ON "ServiceType".id = s.type
        WHERE actiontbl.type = "ActionType".id
            AND actiontbl.result = "ActionResult".id
        ) row

    UNION ALL

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
        SELECT
            'action' as "type",
            calltbl.caseId AS caseId,
            actiontbl.closeTime AS datetime,
            actiontbl.assignedTo AS userId,
            "ActionType".label AS actionType,
            "ActionResult".label AS actionResult,
            actiontbl.comment AS actionComment
        FROM "ActionResult",
            "ActionType",
            actiontbl,
            calltbl
        WHERE actiontbl.type = "ActionType".id
            AND actiontbl.result = "ActionResult".id
            AND actiontbl.callId = calltbl.id
        ) row

    UNION ALL

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
        SELECT
            'call' as "type",
            casetbl.id AS caseId,
            calltbl.callDate AS datetime,
            calltbl.callTaker AS userId,
            "CallType".label AS callType
        FROM calltbl,
            casetbl,
            "CallType"
        WHERE casetbl.contact_phone1 = calltbl.callerPhone
            AND calltbl.callerPhone <> ''
            AND calltbl.callType = "CallType".id
        ) row

    UNION ALL

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
      SELECT
          'comment' as "type", cl.id,
          cs.id as caseId,
          cl.callDate as datetime,
          cl.callTaker as userId,
          cl.customercomment as commentText
        FROM casetbl cs
          left join calltbl cl on (cl.callerPhone = cs.contact_phone1)
        WHERE length(cl.callerPhone) > 7
        AND cl.callerPhone <> '+70000000000'
        AND cl.customercomment is not null
        AND cl.callDate < cs.calldate
        AND cl.callDate >
              coalesce(
                  (select max(xx.calldate)
                    from calltbl xx
                    where xx.callerPhone = cl.callerPhone
                      and xx.caseid <> cs.id
                      and xx.calldate < cs.calldate),
                  '2000-01-01')
      ) row

    UNION ALL

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
        SELECT
            'comment' as "type",
            caseId AS caseId,
            ctime AS datetime,
            author AS userId,
            comment AS commentText
        FROM "CaseComment"
        ) row

    UNION ALL

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
        SELECT
            'partnerDelay' as "type",
            caseId AS caseId,
            ctime AS datetime,
            "PartnerDelay".owner AS userId,
            serviceId AS serviceId,
            "ServiceType".label AS serviceLabel,
            partnertbl.name AS partnername,
            delayminutes,
            "PartnerDelay_Confirmed".label as delayconfirmed
        FROM "PartnerDelay"
          JOIN servicetbl ON (servicetbl.id = serviceid)
          JOIN "ServiceType" ON ("ServiceType".id = servicetbl.type)
          JOIN partnertbl ON (partnertbl.id = partnerid)
          JOIN "PartnerDelay_Confirmed" ON ("PartnerDelay_Confirmed".id = delayconfirmed)
        ) row

    UNION ALL

    -- SMS for partners
    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM ( SELECT 'smsForPartner'::TEXT       AS "type"
                , t.caseRef                   AS caseId
                , t.userRef                   AS userId
                , t.ctime                     AS datetime
                , t.mtime                     AS mtime
                , t.id                        AS id
                , t.msgText                   AS msgText
                , t.sender                    AS sender
                , t.phone                     AS phone
                , GetSmsStatusLabel(t.status) AS deliveryStatus

           FROM "Sms" AS t
           WHERE t.template = 15 -- SmsTemplate id#15 - "notifyPartner"
         ) AS row

    UNION ALL

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
        SELECT
            'partnerCancel' as "type",
            "PartnerCancel".caseId AS caseId,
            "PartnerCancel".ctime AS datetime,
            "PartnerCancel".OWNER AS userId,
            partnertbl.NAME AS partnerName,
            "PartnerRefusalReason".label AS refusalReason,
            NULLIF("PartnerCancel".comment, '') AS refusalComment
        FROM "PartnerCancel",
            "PartnerRefusalReason",
            partnertbl
        WHERE "PartnerCancel".partnercancelreason = "PartnerRefusalReason".id
            AND "PartnerCancel".partnerId = partnertbl.id
        ) row

    UNION ALL

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
        SELECT
            'avayaEvent' as "type",
            actiontbl.caseId AS caseId,
            "AvayaEvent".ctime AS datetime,
            "AvayaEvent".operator AS userId,
            "AvayaEventType".label AS aeType,
            "AvayaEvent".interlocutors AS aeInterlocutors,
            "AvayaEvent".callId AS aeCall
        FROM "AvayaEvent",
            "AvayaEventType",
            actiontbl
        WHERE "AvayaEvent".currentAction = actiontbl.id
            AND "AvayaEventType".id = "AvayaEvent".etype
        ) row

    UNION ALL

    -- Era Glonass incoming Call Cards
    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM ( SELECT 'eraGlonassIncomingCallCard'::TEXT AS "type"
                , t.caseId                           AS caseId
                , t.ctime                            AS datetime
                , casetbl.callTaker                  AS userId
                , t.id                               AS id
                , t.callCardId                       AS "callCardId"
                , t.responseId                       AS "responseId"
                , t.requestBody                      AS "requestBody"

           FROM "CaseEraGlonassCreateRequest" AS t
           INNER JOIN casetbl ON casetbl.id = t.caseId
         ) AS row

    UNION ALL

    SELECT row.caseId
         , row.datetime
         , row.userId
         , ROW_TO_JSON(row)

    FROM (
        SELECT
            'avayaEvent' as "type",
            calltbl.caseId AS caseId,
            "AvayaEvent".ctime AS datetime,
            "AvayaEvent".operator AS userId,
            "AvayaEventType".label AS aeType,
            "AvayaEvent".interlocutors AS aeInterlocutors,
            "AvayaEvent".callId AS aeCall
        FROM "AvayaEvent",
            "AvayaEventType",
            actiontbl,
            calltbl
        WHERE "AvayaEvent".currentAction = actiontbl.id
            AND "AvayaEventType".id = "AvayaEvent".etype
            AND actiontbl.callId = calltbl.id
        ) row

    ) AS united,
    usermetatbl

  WHERE usermetatbl.id = united.userId
  ORDER BY datetime DESC;

GRANT SELECT ON "CaseHistory" TO carma_db_sync;

COMMIT;
