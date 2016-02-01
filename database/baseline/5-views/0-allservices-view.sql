CREATE OR REPLACE VIEW allservicesview AS
  SELECT
      (json->>'id')::int4 as id,
      (json->>'type')::int4 as type,
      (json->>'towdealer_partner') as towdealer_partner,
      (json->>'suburbanmilage') as suburbanmilage,
      (json->>'providedfor') as providedfor,
      (json->>'repairenddate')::timestamp with time zone as repairenddate,
      (json->>'whattosay1') as whatToSay1,
      (json->>'constype')::int4 as consType,
      (json->>'consresult')::int4 as consResult,
      (json->>'consultant')::int4 as consultant,
      (json->>'techtype')::int4 as techtype,
      (json->>'towtype')::int4 as towtype,
      (json->>'towertype')::int4 as towertype,
      (json->>'towaddress_address') as towaddress_address,
      (json->>'towdealer_partnerid')::int4 as towdealer_partnerid,
      (json->>'parentid')::int4 as parentid,
      (json->'flags')::json as flags
    FROM (
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              suburbanmilage,
              contractor_address AS towaddress_address,
              contractor_partnerid AS towdealer_partnerid,
              parentid
            FROM averagecommissionertbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              suburbanmilage,
              contractor_partnerid AS towdealer_partnerid,
              parentid
             FROM banktbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              suburbanmilage,
              whattosay1,
              constype,
              consresult,
              consultant,
              contractor_partnerid AS towdealer_partnerid,
              parentid
            FROM consultationtbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              suburbanmilage,
              contractor_address AS towaddress_address,
              contractor_partnerid AS towdealer_partnerid,
              parentid
            FROM deliverclienttbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              suburbanmilage,
              contractor_address AS towaddress_address,
              contractor_partnerid AS towdealer_partnerid,
              parentid
            FROM informationtbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              suburbanmilage,
              contractor_address AS towaddress_address,
              contractor_partnerid AS towdealer_partnerid,
              parentid
            FROM deliverpartstbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              suburbanmilage,
              providedfor,
              contractor_address AS towaddress_address,
              contractor_partnerid AS towdealer_partnerid,
              parentid
            FROM hoteltbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              suburbanmilage,
              contractor_address AS towaddress_address,
              contractor_partnerid AS towdealer_partnerid,
              parentid
            FROM sobertbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              towdealer_partner,
              providedfor,
              towdealer_partnerid,
              parentid
            FROM renttbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              towdealer_partner,
              suburbanmilage,
              repairenddate,
              towtype,
              towertype,
              towaddress_address,
              towdealer_partnerid,
              parentid,
              (select row_to_json(flags)
                from (values (check1, check2))
                  as flags(
                    "Заблокирован электронный ручной тормоз",
                    "Руль заблокирован")
                ) as flags
            FROM towagetbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              suburbanmilage,
              techtype,
              parentid,
              (select row_to_json(flags)
                from (values(check1, check2, check3, check4, check5, check6))
                  as flags(
                    "Капот открывается",
                    "Наличие запасного колеса",
                    "Наличие секреток",
                    "Запасной ключ имеется",
                    "Документы на автомобиль на руках",
                    "Не открывается лючок бензобака")
                ) as flags
              FROM techtbl) x
      UNION ALL
        SELECT row_to_json(x.*) AS json FROM
          (SELECT
              id,
              type,
              contractor_partner AS towdealer_partner,
              taxito_address AS towaddress_address,
              contractor_partnerid AS towdealer_partnerid,
              parentid
              FROM taxitbl) x
    ) res(json);
