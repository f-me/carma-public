DROP VIEW "Услуги";
DROP VIEW "Отказы партнеров";
DROP VIEW "Партнеры";
DROP VIEW "Услуги с приоритетами";
DROP VIEW partnercancelview;
DROP VIEW servicesview;
DROP VIEW allservicesview;

-- remap service types
ALTER TABLE servicetbl ADD COLUMN type_tmp int4;
UPDATE servicetbl SET type_tmp = n.id
FROM "ServiceNames" n WHERE n.value = type;
ALTER TABLE servicetbl ALTER COLUMN type SET NOT NULL;

ALTER TABLE servicetbl DROP COLUMN type;
ALTER TABLE consultationtbl DROP COLUMN type;
ALTER TABLE tech1tbl DROP COLUMN type;

ALTER TABLE servicetbl ADD COLUMN type int4 REFERENCES "ServiceType";
UPDATE servicetbl SET type = type_tmp;
DELETE FROM servicetbl WHERE type IS NULL;
ALTER TABLE servicetbl ALTER COLUMN type SET NOT NULL;
ALTER TABLE servicetbl DROP COLUMN type_tmp;

-- Rebind from ServiceNames to ServiceType dictionary
ALTER TABLE "ServiceInfo" DROP CONSTRAINT "ServiceInfo_service_fkey";
ALTER TABLE "ServiceInfo" ADD CONSTRAINT "ServiceInfo_service_fkey"
FOREIGN KEY (service) REFERENCES "ServiceType" (id);

ALTER TABLE servicetbl
ADD CONSTRAINT "servicetbl_type_id_unique" UNIQUE (type,id);

ALTER TABLE "SubProgramService" DROP CONSTRAINT "SubProgramService_type_fkey";
ALTER TABLE "SubProgramService" ADD CONSTRAINT "SubProgramService_type_fkey"
FOREIGN KEY (type) REFERENCES "ServiceType" (id);

-- remap partner_service types
ALTER TABLE partner_servicetbl ADD COLUMN type_tmp int4;
UPDATE partner_servicetbl SET type_tmp = n.id
FROM "ServiceNames" n WHERE n.value = servicename;
ALTER TABLE partner_servicetbl DROP COLUMN servicename;
ALTER TABLE partner_servicetbl ADD COLUMN servicename int4 REFERENCES "ServiceType";

-- clean orphans and unrecognized services
DELETE FROM partner_servicetbl WHERE parentid IS NULL;
DELETE FROM partner_servicetbl WHERE parentid = '';
UPDATE partner_servicetbl SET servicename = type_tmp;
DELETE FROM partner_servicetbl WHERE servicename IS NULL;
ALTER TABLE partner_servicetbl DROP COLUMN type_tmp;

-- refill all partner.services using cleaned data
UPDATE partnertbl SET services = '';
UPDATE partnertbl SET services = s.services
FROM
(SELECT substring(parentid from 9 for 5)::int as partner,
array_to_string(array_agg('partner_service:' || id::text), ',') as services
FROM partner_servicetbl GROUP BY parentid) s
WHERE id = s.partner;

-- clean broken refusals (all prior July 2013)
DELETE FROM partnercanceltbl WHERE serviceId IS NULL;

-- remap partnerCancel service references
ALTER TABLE partnercanceltbl ADD COLUMN sid_tmp int4;
UPDATE partnercanceltbl SET sid_tmp = n.id
FROM "ServiceNames" n WHERE n.value = split_part(serviceId,':',1);

ALTER TABLE partnercanceltbl ADD COLUMN serviceType int4 REFERENCES "ServiceType";
INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES (1, 'partnerCancel', 'serviceType', 'true', 'true');
UPDATE partnercanceltbl SET sid_tmp = split_part(serviceId,':',2)::int;

ALTER TABLE partnercanceltbl DROP COLUMN serviceId;
ALTER TABLE partnercanceltbl ADD COLUMN serviceId int4;
UPDATE partnercanceltbl SET serviceId = sid_tmp;
ALTER TABLE partnercanceltbl DROP COLUMN sid_tmp;
