CREATE OR REPLACE VIEW allservicesview AS
                (((        (        (        (        (        (        (        (         SELECT averagecommissionertbl.id,

                                                                                            averagecommissionertbl.type,
                                                                                            averagecommissionertbl.contractor_partner AS towdealer_partner,
                                                                                            averagecommissionertbl.suburbanmilage,
                                                                                            NULL::text AS providedfor,
                                                                                            NULL::timestamp with time zone AS repairenddate,
                                                                                            NULL::text AS whatToSay1,
                                                                                            NULL::int4 AS consType,
                                                                                            NULL::int4 AS consResult,
                                                                                            NULL::int4 AS consultant,
                                                                                            NULL::int4 AS techtype,
                                                                                            NULL::int4 AS towtype,
                                                                                            NULL::int4 AS towertype,
                                                                                            averagecommissionertbl.contractor_address AS towaddress_address,
                                                                                            averagecommissionertbl.contractor_partnerid AS towdealer_partnerid,
                                                                                            averagecommissionertbl.parentid,
                                                                                            '{}'::json AS flags
                                                                                           FROM averagecommissionertbl
                                                                                UNION ALL
                                                                                         SELECT banktbl.id,

                                                                                            banktbl.type,
                                                                                            banktbl.contractor_partner AS towdealer_partner,
                                                                                            banktbl.suburbanmilage,
                                                                                            NULL::text AS providedfor,
                                                                                            NULL::timestamp with time zone AS repairenddate,
                                                                                            NULL::text AS whatToSay1,
                                                                                            NULL::int4 AS consType,
                                                                                            NULL::int4 AS consResult,
                                                                                            NULL::int4 AS consultant,
                                                                                            NULL::int4 AS techtype,
                                                                                            NULL::int4 AS towtype,
                                                                                            NULL::int4 AS towertype,
                                                                                            NULL::text AS towaddress_address,
                                                                                            banktbl.contractor_partnerid AS towdealer_partnerid,
                                                                                            banktbl.parentid,
                                                                                            '{}'::json AS flags
                                                                                           FROM banktbl)
                                                                        UNION ALL
                                                                                 SELECT consultationtbl.id,

                                                                                    consultationtbl.type,
                                                                                    consultationtbl.contractor_partner AS towdealer_partner,
                                                                                    consultationtbl.suburbanmilage,
                                                                                    NULL::text AS providedfor,
                                                                                    NULL::timestamp with time zone AS repairenddate,
                                                                                    consultationtbl.whatToSay1,
                                                                                    consultationtbl.consType,
                                                                                    consultationtbl.consResult,
                                                                                    consultationtbl.consultant,
                                                                                    NULL::int4 AS techtype,
                                                                                    NULL::int4 AS towtype,
                                                                                    NULL::int4 AS towertype,
                                                                                    NULL::text AS towaddress_address,
                                                                                    consultationtbl.contractor_partnerid AS towdealer_partnerid,
                                                                                    consultationtbl.parentid,
                                                                                    '{}'::json AS flags
                                                                                   FROM consultationtbl)
                                                                UNION ALL
                                                                         SELECT deliverclienttbl.id,

                                                                            deliverclienttbl.type,
                                                                            deliverclienttbl.contractor_partner AS towdealer_partner,
                                                                            deliverclienttbl.suburbanmilage,
                                                                            NULL::text AS providedfor,
                                                                            NULL::timestamp with time zone AS repairenddate,
                                                                            NULL::text AS whatToSay1,
                                                                            NULL::int4 AS consType,
                                                                            NULL::int4 AS consResult,
                                                                            NULL::int4 AS consultant,
                                                                            NULL::int4 AS techtype,
                                                                            NULL::int4 AS towtype,
                                                                            NULL::int4 AS towertype,
                                                                            deliverclienttbl.contractor_address AS towaddress_address,
                                                                            deliverclienttbl.contractor_partnerid AS towdealer_partnerid,
                                                                            deliverclienttbl.parentid,
                                                                            '{}'::json AS flags
                                                                           FROM deliverclienttbl)
                                                        UNION ALL
                                                                 SELECT informationtbl.id,
                                                                    informationtbl.type,
                                                                    informationtbl.contractor_partner AS towdealer_partner,
                                                                    informationtbl.suburbanmilage,
                                                                    NULL::text AS providedfor,
                                                                    NULL::timestamp with time zone AS repairenddate,
                                                                    NULL::text AS whatToSay1,
                                                                    NULL::int4 AS consType,
                                                                    NULL::int4 AS consResult,
                                                                    NULL::int4 AS consultant,
                                                                    NULL::int4 AS techtype,
                                                                    NULL::int4 AS towtype,
                                                                    NULL::int4 AS towertype,
                                                                    informationtbl.contractor_address AS towaddress_address,
                                                                    informationtbl.contractor_partnerid AS towdealer_partnerid,
                                                                    informationtbl.parentid,
                                                                    '{}'::json AS flags
                                                                   FROM informationtbl)
                                                UNION ALL
                                                         SELECT deliverpartstbl.id,

                                                            deliverpartstbl.type,
                                                            deliverpartstbl.contractor_partner AS towdealer_partner,
                                                            deliverpartstbl.suburbanmilage,
                                                            NULL::text AS providedfor,
                                                            NULL::timestamp with time zone AS repairenddate,
                                                            NULL::text AS whatToSay1,
                                                            NULL::int4 AS consType,
                                                            NULL::int4 AS consResult,
                                                            NULL::int4 AS consultant,
                                                            NULL::int4 AS techtype,
                                                            NULL::int4 AS towtype,
                                                            NULL::int4 AS towertype,
                                                            deliverpartstbl.contractor_address AS towaddress_address,
                                                            deliverpartstbl.contractor_partnerid AS towdealer_partnerid,
                                                            deliverpartstbl.parentid,
                                                            '{}'::json AS flags
                                                           FROM deliverpartstbl)
                                        UNION ALL
                                                 SELECT hoteltbl.id,

                                                    hoteltbl.type,
                                                    hoteltbl.contractor_partner AS towdealer_partner,
                                                    hoteltbl.suburbanmilage,
                                                    hoteltbl.providedfor,
                                                    NULL::timestamp with time zone AS repairenddate,
                                                    NULL::text AS whatToSay1,
                                                    NULL::int4 AS consType,
                                                    NULL::int4 AS consResult,
                                                    NULL::int4 AS consultant,
                                                    NULL::int4 AS techtype,
                                                    NULL::int4 AS towtype,
                                                    NULL::int4 AS towertype,
                                                    hoteltbl.contractor_address AS towaddress_address,
                                                    hoteltbl.contractor_partnerid AS towdealer_partnerid,
                                                    hoteltbl.parentid,
                                                    '{}'::json AS flags
                                                   FROM hoteltbl)
                                UNION ALL
                                         SELECT sobertbl.id,
                                            sobertbl.type,
                                            sobertbl.contractor_partner AS towdealer_partner,
                                            sobertbl.suburbanmilage,
                                            NULL::text AS providedfor,
                                            NULL::timestamp with time zone AS repairenddate,
                                            NULL::text AS whatToSay1,
                                            NULL::int4 AS consType,
                                            NULL::int4 AS consResult,
                                            NULL::int4 AS consultant,
                                            NULL::int4 AS techtype,
                                            NULL::int4 AS towtype,
                                            NULL::int4 AS towertype,
                                            sobertbl.contractor_address AS towaddress_address,
                                            sobertbl.contractor_partnerid AS towdealer_partnerid,
                                            sobertbl.parentid,
                                            '{}'::json AS flags
                                           FROM sobertbl)
                        UNION ALL
                                 SELECT renttbl.id,
                                    renttbl.type,
                                    renttbl.towdealer_partner,
                                    NULL::text AS suburbanmilage,
                                    renttbl.providedfor,
                                    NULL::timestamp with time zone AS repairenddate,
                                    NULL::text AS whatToSay1,
                                    NULL::int4 AS consType,
                                    NULL::int4 AS consResult,
                                    NULL::int4 AS consultant,
                                    NULL::int4 AS techtype,
                                    NULL::int4 AS towtype,
                                    NULL::int4 AS towertype,
                                    NULL::text AS towaddress_address,
                                    renttbl.towdealer_partnerid,
                                    renttbl.parentid,
                                    '{}'::json AS flags
                                   FROM renttbl)
                UNION ALL
                         SELECT towagetbl.id,
                            towagetbl.type,
                            towagetbl.towdealer_partner,
                            towagetbl.suburbanmilage,
                            NULL::text AS providedfor,
                            towagetbl.repairenddate,
                            NULL::text AS whatToSay1,
                            NULL::int4 AS consType,
                            NULL::int4 AS consResult,
                            NULL::int4 AS consultant,
                            NULL::int4 AS techtype,
                            towagetbl.towtype,
                            towagetbl.towertype,
                            towagetbl.towaddress_address,
                            towagetbl.towdealer_partnerid,
                            towagetbl.parentid,
                            (select row_to_json(flags)
                              from (values (towagetbl.check1, towagetbl.check2))
                                as flags(
                                    "Заблокирован электронный ручной тормоз",
                                    "Руль заблокирован")
                            ) as flags
                           FROM towagetbl)
        UNION ALL
                 SELECT techtbl.id,
                    techtbl.type,
                    NULL::text AS towdealer_partner,
                    techtbl.suburbanmilage,
                    NULL::text AS providedfor,
                    NULL::timestamp with time zone AS repairenddate,
                    NULL::text AS whatToSay1,
                    NULL::int4 AS consType,
                    NULL::int4 AS consResult,
                    NULL::int4 AS consultant,
                    techtbl.techtype,
                    NULL::int4 AS towtype,
                    NULL::int4 AS towertype,
                    NULL::text AS towaddress_address,
                    NULL::int4 AS towdealer_partnerid,
                    techtbl.parentid,
                    (select row_to_json(flags)
                      from (values(
                            techtbl.check1, techtbl.check2, techtbl.check3,
                            techtbl.check4, techtbl.check5, techtbl.check6))
                        as flags(
                          "Капот открывается",
                          "Наличие запасного колеса",
                          "Наличие секреток",
                          "Запасной ключ имеется",
                          "Документы на автомобиль на руках",
                          "Не открывается лючок бензобака")
                    ) as flags
                   FROM techtbl)
UNION ALL
         SELECT taxitbl.id,
            taxitbl.type,
            taxitbl.contractor_partner AS towdealer_partner,
            NULL::text AS suburbanmilage,
            NULL::text AS providedfor,
            NULL::timestamp with time zone AS repairenddate,
            NULL::text AS whatToSay1,
            NULL::int4 AS consType,
            NULL::int4 AS consResult,
            NULL::int4 AS consultant,
            NULL::int4 AS techtype,
            NULL::int4 AS towtype,
            NULL::int4 AS towertype,
            taxitbl.taxito_address AS towaddress_address,
            taxitbl.contractor_partnerid AS towdealer_partnerid,
            taxitbl.parentid,
            '{}'::json AS flags
           FROM taxitbl;
