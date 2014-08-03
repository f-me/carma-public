DROP VIEW "Услуги";
DROP VIEW "Отказы партнеров";
DROP VIEW servicesview;
DROP VIEW allservicesview;

ALTER TABLE servicetbl ADD COLUMN type_tmp int4;
UPDATE servicetbl SET type_tmp = n.id
FROM "ServiceNames" n WHERE n.value = type;
ALTER TABLE servicetbl ALTER COLUMN type SET NOT NULL;

ALTER TABLE servicetbl DROP COLUMN type;
ALTER TABLE servicetbl ADD COLUMN type int4 REFERENCES "ServiceType";
UPDATE servicetbl SET type = type_tmp;
ALTER TABLE servicetbl DROP COLUMN type_tmp;

-- Rebind from ServiceNames to ServiceType dictionary
ALTER TABLE "ServiceInfo" DROP CONSTRAINT "ServiceInfo_service_fkey";
ALTER TABLE "ServiceInfo" ADD CONSTRAINT "ServiceInfo_service_fkey"
FOREIGN KEY (service) REFERENCES "ServiceType" (id);

ALTER TABLE "SubProgramService" DROP CONSTRAINT "SubProgramService_type_fkey";
ALTER TABLE "SubProgramService" ADD CONSTRAINT "SubProgramService_type_fkey"
FOREIGN KEY (type) REFERENCES "ServiceType" (id);
