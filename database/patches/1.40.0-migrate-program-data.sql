-- Transfer basic data to Program
UPDATE "Program" n
SET client=p.client,
    clientaddress=p.clientaddress,
    clientcode=p.clientcode
FROM programtbl p, "SubProgram" s
WHERE s.value = p.value AND s.parent = n.id;

-- Transfer basic data to SubProgram
UPDATE "SubProgram" n
SET checkPeriod = p.carCheckPeriodDefault,
    validFor = p.duedateDefault,
    logo = p.logo,
    template = p.contracts,
    help = p.help,
    active = p.active
FROM programtbl p
WHERE n.value = p.value;

INSERT INTO "SubProgramService" (parent, type)
WITH remap AS
-- programtbl.value-serviceType map
(SELECT value as program, unnest(services) AS type FROM programtbl)
SELECT p.id AS parent, s.id AS type FROM remap
-- Convert old values to new ServiceNames dictionary
JOIN "ServiceNames" s ON s.value=remap.type
-- Map to new SubProgram entries
JOIN "SubProgram" p ON p.value=remap.program
ORDER BY parent, s.id;

-- Now store inverse 1-to-many links in SubPrograms so that the client
-- can pull them:
UPDATE "SubProgram" s SET services=remap.services
FROM
(SELECT s.id, array_agg(v.id) as services
FROM "SubProgram" s, "SubProgramService" v
WHERE v.parent=s.id
GROUP BY s.id
ORDER BY id) remap
WHERE s.id=remap.id;
