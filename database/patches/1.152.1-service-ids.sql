CREATE SEQUENCE servicetbl_sid_seq;

-- Unique id for all services (and inherited tables)
ALTER TABLE servicetbl DROP CONSTRAINT "servicetbl_pkey",
ADD COLUMN sid int4 DEFAULT nextval('servicetbl_sid_seq');

ALTER TABLE servicetbl ADD CONSTRAINT "servicetbl_ukey" UNIQUE (sid);

-- Delete duplicates (keep earliest service)
DELETE FROM servicetbl
WHERE sid IN (SELECT sid
              FROM (SELECT sid,
                             row_number() over (partition BY id,type ORDER BY sid) AS rnum
                     FROM servicetbl) t
              WHERE t.rnum > 1);

ALTER TABLE servicetbl
ADD modelName text NOT NULL DEFAULT '';

-- Migrate casetbl services references
UPDATE servicetbl SET modelName = 'Tech' where type = 1;
UPDATE servicetbl SET modelName = 'Towage' where type =  2;
UPDATE servicetbl SET modelName = 'Rent' where type =  3;
UPDATE servicetbl SET modelName = 'Hotel' where type = 4;
UPDATE servicetbl SET modelName = 'Taxi' where type = 5;
UPDATE servicetbl SET modelName = 'SoberDriver' where type = 6;
UPDATE servicetbl SET modelName = 'Transportation' where type = 7;
UPDATE servicetbl SET modelName = 'DeliverCar' where type = 8;
UPDATE servicetbl SET modelName = 'DeliverParts' where type = 9;
UPDATE servicetbl SET modelName = 'LegalAssistance' where type =10;
UPDATE servicetbl SET modelName = 'TechInspect' where type =11;
UPDATE servicetbl SET modelName = 'Information' where type =12;
UPDATE servicetbl SET modelName = 'Consultation' where type =13;
UPDATE servicetbl SET modelName = 'Tickets' where type =14;
UPDATE servicetbl SET modelName = 'Continue' where type =15;
UPDATE servicetbl SET modelName = 'Bank' where type =16;
UPDATE servicetbl SET modelName = 'DeliverClient' where type =17;
UPDATE servicetbl SET modelName = 'AverageCommissioner' where type =18;
UPDATE servicetbl SET modelName = 'Insurance' where type =19;
UPDATE servicetbl SET modelName = modelName || ':' || sid::text;
CREATE TEMPORARY TABLE casetbl_services AS
SELECT parentId as caseId,array_to_string(array_agg(modelName), ',') as services
FROM servicetbl GROUP BY parentid;

UPDATE casetbl SET services = null;

UPDATE casetbl c SET services = q.services
FROM casetbl_services q WHERE q.caseId = id;

-- Replace old service ids with new unique sid

DROP VIEW IF EXISTS "Отказы партнеров";
DROP VIEW IF EXISTS servicesview;
DROP VIEW IF EXISTS "Услуги";
DROP VIEW IF EXISTS allservicesview;

CREATE OR REPLACE FUNCTION serviceid_fk() RETURNS TRIGGER AS $$
BEGIN
    IF NOT EXISTS(SELECT 1 FROM servicetbl WHERE id = new.service_id) then
        RAISE EXCEPTION 'No such service: %', new.service_id;
    END IF;
    RETURN new;
end;
$$ language plpgsql;

-- migrate servicetbl refs
UPDATE actiontbl p SET serviceId = q.sid
FROM servicetbl q WHERE serviceId = q.id AND serviceType = q.type;
ALTER TABLE actiontbl DROP COLUMN serviceType;

UPDATE partnercanceltbl p SET serviceId = q.sid
FROM servicetbl q WHERE serviceId = q.id AND serviceType = q.type;
ALTER TABLE partnercanceltbl DROP COLUMN serviceType;

ALTER TABLE servicetbl DROP COLUMN modelName;
ALTER TABLE servicetbl DROP COLUMN id;
ALTER TABLE tech1tbl DROP COLUMN id;
ALTER TABLE consultationtbl DROP COLUMN id;

-- migrate servicetbl
ALTER TABLE servicetbl ADD COLUMN id int4;
UPDATE servicetbl SET id = sid;
ALTER TABLE servicetbl ADD CONSTRAINT "servicetbl_pkey" PRIMARY KEY (id);
ALTER TABLE servicetbl DROP COLUMN sid;

-- setup FK triggers (servicetbl has children thus no native FKs)
CREATE CONSTRAINT TRIGGER actiontbl_serviceid_fk
AFTER INSERT OR UPDATE ON actiontbl
FOR EACH ROW EXECUTE PROCEDURE serviceid_fk();

CREATE CONSTRAINT TRIGGER partnercanceltbl_serviceid_fk
AFTER INSERT OR UPDATE ON partnercanceltbl
FOR EACH ROW EXECUTE PROCEDURE serviceid_fk();
