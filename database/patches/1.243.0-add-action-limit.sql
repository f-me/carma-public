ALTER TABLE "ActionType"
ADD COLUMN maxSeconds int4 NOT NULL DEFAULT 300;

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'ActionType', 'maxSeconds', r, w FROM "FieldPermission" WHERE model='ActionType' AND field='priority');
