CREATE VIEW "CaseHistory"
AS
SELECT caseId,
    datetime,
    usermetatbl.realName || ' (' || usermetatbl.login || ')' AS who,
    row_to_json AS json
FROM (
    SELECT row.caseId,
        row.datetime,
        row.userId,
        row_to_json(row)
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
            "ServiceType".label AS serviceLabel
        FROM "ActionResult",
            "ActionType",
            actiontbl
        LEFT OUTER JOIN servicetbl s ON s.id = actiontbl.serviceId
        LEFT JOIN "ServiceType" ON "ServiceType".id = s.type
        WHERE actiontbl.type = "ActionType".id
            AND actiontbl.result = "ActionResult".id
        ) row

    UNION ALL

    SELECT row.caseId,
        row.datetime,
        row.userId,
        row_to_json(row)
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

    SELECT
        row.caseId,
        row.datetime,
        row.userId,
        row_to_json(row)
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

    SELECT row.caseId,
        row.datetime,
        row.userId,
        row_to_json(row)
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

    SELECT row.caseId,
        row.datetime,
        row.userId,
        row_to_json(row)
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

    SELECT row.caseId,
        row.datetime,
        row.userId,
        row_to_json(row)
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

    SELECT row.caseId,
        row.datetime,
        row.userId,
        row_to_json(row)
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
    ) united,
    usermetatbl
WHERE usermetatbl.id = united.userId
ORDER BY datetime DESC;

GRANT SELECT ON "CaseHistory" TO carma_db_sync;
