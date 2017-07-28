BEGIN;

/*
  carma=# select id, value, label from "Role" where id = 19;
   id |  value  |             label
  ----+---------+--------------------------------
   19 | partner | Пользователь экрана контрактов
  (1 row)
*/
UPDATE "FieldPermission" SET w = FALSE
  WHERE model = 'Contract'
    AND field = 'subprogram'
    AND role  = 19
        ;

COMMIT;
