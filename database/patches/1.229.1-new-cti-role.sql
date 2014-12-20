INSERT INTO "Role" (id, value, label, isBack)
VALUES (50, 'cti', 'Доступ к CTI-панели', 'f');

SELECT setval(pg_get_serial_sequence('"Role"', 'id'), max(id)) from "Role";
