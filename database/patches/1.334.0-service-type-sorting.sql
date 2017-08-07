BEGIN;

CREATE SEQUENCE "ServiceType_ordering_seq";
ALTER TABLE "ServiceType" ADD COLUMN ordering int4 DEFAULT nextval('"ServiceType_ordering_seq"');
GRANT ALL ON "ServiceType_ordering_seq" TO carma_db_sync;

UPDATE "ServiceType" SET ordering = ordering + 1 WHERE ordering > 2;
UPDATE "ServiceType" SET ordering = 3 WHERE id = 19;

COMMIT;
