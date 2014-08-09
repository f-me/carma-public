ALTER TABLE "ServiceStatus" ADD COLUMN button bool NOT NULL DEFAULT false;

ALTER TABLE "CaseStatus" ADD COLUMN button bool NOT NULL DEFAULT false;
INSERT INTO "CaseStatus" (id, label) VALUES
(6, 'Заказ услуги через мобильное приложение');
SELECT setval(pg_get_serial_sequence('"CaseStatus"', 'id'), max(id)) from "CaseStatus";


INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
(6, 'ServiceStatus', 'button', 'true', 'true');

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
(7, 'ServiceStatus', 'button', 'true', 'false');

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
(6, 'CaseStatus', 'button', 'true', 'true');

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
(7, 'CaseStatus', 'button', 'true', 'false');
