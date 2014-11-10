INSERT INTO "Role" (id, value, label, isBack)
VALUES (43, 'bo_info', 'БО: Заказ услуги (ТДИ)', 't');

update usermetatbl set roles = roles || array[43] where 23 = any(roles);

SELECT setval(pg_get_serial_sequence('"Role"', 'id'), max(id)) from "Role";

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Уточнить время выезда', 21, 1);

SELECT setval(pg_get_serial_sequence('"ActionType"', 'id'), max(id)) from "ActionType";
