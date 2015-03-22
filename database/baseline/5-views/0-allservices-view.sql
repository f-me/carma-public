CREATE OR REPLACE VIEW allservicesview AS
                (((        (        (        (        (        (        (        (         SELECT averagecommissionertbl.id,

                                                                                            averagecommissionertbl.type,
                                                                                            averagecommissionertbl.contractor_partner AS towdealer_partner,
                                                                                            averagecommissionertbl.suburbanmilage,
                                                                                            NULL::text AS providedfor,
                                                                                            NULL::timestamp with time zone AS repairenddate,
                                                                                            NULL::text as whatToSay1,
                                                                                            NULL::int4 as consType,
                                                                                            NULL::int4 AS techtype,
                                                                                            NULL::int4 AS towtype,
                                                                                            averagecommissionertbl.contractor_address AS towaddress_address,
                                                                                            averagecommissionertbl.contractor_partnerid AS towdealer_partnerid,
                                                                                            averagecommissionertbl.parentid
                                                                                           FROM averagecommissionertbl
                                                                                UNION ALL
                                                                                         SELECT banktbl.id,

                                                                                            banktbl.type,
                                                                                            banktbl.contractor_partner AS towdealer_partner,
                                                                                            banktbl.suburbanmilage,
                                                                                            NULL::text AS providedfor,
                                                                                            NULL::timestamp with time zone AS repairenddate,
                                                                                            NULL::text as whatToSay1,
                                                                                            NULL::int4 as consType,
                                                                                            NULL::int4 AS techtype,
                                                                                            NULL::int4 AS towtype,
                                                                                            NULL::text AS towaddress_address,
                                                                                            banktbl.contractor_partnerid AS towdealer_partnerid,
                                                                                            banktbl.parentid
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
                                                                                    NULL::int4 AS techtype,
                                                                                    NULL::int4 AS towtype,
                                                                                    NULL::text AS towaddress_address,
                                                                                    consultationtbl.contractor_partnerid AS towdealer_partnerid,
                                                                                    consultationtbl.parentid
                                                                                   FROM consultationtbl)
                                                                UNION ALL
                                                                         SELECT deliverclienttbl.id,

                                                                            deliverclienttbl.type,
                                                                            deliverclienttbl.contractor_partner AS towdealer_partner,
                                                                            deliverclienttbl.suburbanmilage,
                                                                            NULL::text AS providedfor,
                                                                            NULL::timestamp with time zone AS repairenddate,
                                                                            NULL::text as whatToSay1,
                                                                            NULL::int4 as consType,
                                                                            NULL::int4 AS techtype,
                                                                            NULL::int4 AS towtype,
                                                                            deliverclienttbl.contractor_address AS towaddress_address,
                                                                            deliverclienttbl.contractor_partnerid AS towdealer_partnerid,
                                                                            deliverclienttbl.parentid
                                                                           FROM deliverclienttbl)
                                                        UNION ALL
                                                                 SELECT informationtbl.id,

                                                                    informationtbl.type,
                                                                    informationtbl.contractor_partner AS towdealer_partner,
                                                                    informationtbl.suburbanmilage,
                                                                    NULL::text AS providedfor,
                                                                    NULL::timestamp with time zone AS repairenddate,
                                                                    NULL::text as whatToSay1,
                                                                    NULL::int4 as consType,
                                                                    NULL::int4 AS techtype,
                                                                    NULL::int4 AS towtype,
                                                                    informationtbl.contractor_address AS towaddress_address,
                                                                    informationtbl.contractor_partnerid AS towdealer_partnerid,
                                                                    informationtbl.parentid
                                                                   FROM informationtbl)
                                                UNION ALL
                                                         SELECT deliverpartstbl.id,

                                                            deliverpartstbl.type,
                                                            deliverpartstbl.contractor_partner AS towdealer_partner,
                                                            deliverpartstbl.suburbanmilage,
                                                            NULL::text AS providedfor,
                                                            NULL::timestamp with time zone AS repairenddate,
NULL::text as whatToSay1,
                                                            NULL::int4 as consType,
                                                            NULL::int4 AS techtype,
                                                            NULL::int4 AS towtype,
                                                            deliverpartstbl.contractor_address AS towaddress_address,
                                                            deliverpartstbl.contractor_partnerid AS towdealer_partnerid,
                                                            deliverpartstbl.parentid
                                                           FROM deliverpartstbl)
                                        UNION ALL
                                                 SELECT hoteltbl.id,

                                                    hoteltbl.type,
                                                    hoteltbl.contractor_partner AS towdealer_partner,
                                                    hoteltbl.suburbanmilage,
                                                    hoteltbl.providedfor,
                                                    NULL::timestamp with time zone AS repairenddate,
                                                    NULL::text as whatToSay1,
                                                    NULL::int4 as consType,
                                                    NULL::int4 AS techtype,
                                                    NULL::int4 AS towtype,
                                                    hoteltbl.contractor_address AS towaddress_address,
                                                    hoteltbl.contractor_partnerid AS towdealer_partnerid,
                                                    hoteltbl.parentid
                                                   FROM hoteltbl)
                        UNION ALL
                                 SELECT sobertbl.id,

                                    sobertbl.type,
                                    sobertbl.contractor_partner AS towdealer_partner,
                                    sobertbl.suburbanmilage,
                                    NULL::text AS providedfor,
                                    NULL::timestamp with time zone AS repairenddate,
                                    NULL::text as whatToSay1,
                                    NULL::int4 as consType,
                                    NULL::int4 AS techtype,
                                    NULL::int4 AS towtype,
                                    sobertbl.contractor_address AS towaddress_address,
                                    sobertbl.contractor_partnerid AS towdealer_partnerid,
                                    sobertbl.parentid
                                   FROM sobertbl)
                UNION ALL
                         SELECT renttbl.id,

                            renttbl.type,
                            renttbl.towdealer_partner,
                            NULL::text AS suburbanmilage,
                            renttbl.providedfor,
                            NULL::timestamp with time zone AS repairenddate,
                            NULL::text as whatToSay1,
                            NULL::int4 as consType,
                            NULL::int4 AS techtype,
                            NULL::int4 AS towtype,
                            NULL::text AS towaddress_address,
                            renttbl.towdealer_partnerid,
                            renttbl.parentid
                           FROM renttbl)
        UNION ALL
                 SELECT towagetbl.id,

                    towagetbl.type,
                    towagetbl.towdealer_partner,
                    towagetbl.suburbanmilage,
                    NULL::text AS providedfor,
                    towagetbl.repairenddate,
                    NULL::text as whatToSay1,
                    NULL::int4 as consType,
                    NULL::int4 AS techtype,
                    towagetbl.towtype,
                    towagetbl.towaddress_address,
                    towagetbl.towdealer_partnerid,
                    towagetbl.parentid
                   FROM towagetbl)
UNION ALL
         SELECT techtbl.id,

            techtbl.type,
            NULL::text AS towdealer_partner,
            techtbl.suburbanmilage,
            NULL::text AS providedfor,
            NULL::timestamp with time zone AS repairenddate,
            NULL::text as whatToSay1,
            NULL::int4 as consType,
            techtbl.techtype,
            NULL::int4 AS towtype,
            NULL::text AS towaddress_address,
            NULL::int4 AS towdealer_partnerid,
            techtbl.parentid
           FROM techtbl)
UNION ALL
SELECT taxitbl.id,
       taxitbl.type,
       taxitbl.contractor_partner AS towdealer_partner,
       NULL::text AS suburbanmilage,
       NULL::text AS providedfor,
       NULL::TIMESTAMP WITH time ZONE AS repairenddate,
       NULL::text as whatToSay1,
       NULL::int4 as consType,
       NULL::integer AS techtype,
       NULL::integer AS towtype,
       taxitbl.taxito_address AS towaddress_address,
       taxitbl.contractor_partnerid AS towdealer_partnerid,
       taxitbl.parentid
FROM taxitbl;
