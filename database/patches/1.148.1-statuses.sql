INSERT INTO "CaseStatus" (id, label) VALUES
(6, 'Заказ услуги через мобильное приложение');
SELECT setval(pg_get_serial_sequence('"CaseStatus"', 'id'), max(id)) from "CaseStatus";
