GRANT ALL ON "ActionResult" TO carma_db_sync;
GRANT ALL ON "ActionResult" TO carma_search;

GRANT ALL ON "ActionType" TO carma_db_sync;
GRANT ALL ON "ActionType" TO carma_search;

ALTER TABLE "ActionType" DROP COLUMN "desc",
ADD COLUMN description text NOT NULL DEFAULT '';

UPDATE "FieldPermission" SET field='description'
WHERE model='ActionType' AND field='desc';
