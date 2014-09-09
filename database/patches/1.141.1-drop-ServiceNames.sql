ALTER TABLE "SubProgramService"
DROP CONSTRAINT "SubProgramService_type_fkey";

ALTER TABLE "ServiceInfo"
DROP CONSTRAINT "ServiceInfo_service_fkey";

DROP TABLE "ServiceNames";

ALTER TABLE "SubProgramService"
ADD CONSTRAINT "SubProgramService_type_fkey"
FOREIGN KEY (type) REFERENCES "ServiceType" (id);

ALTER TABLE "ServiceInfo"
ADD CONSTRAINT "ServiceInfo_service_fkey"
FOREIGN KEY (service) REFERENCES "ServiceType" (id);
