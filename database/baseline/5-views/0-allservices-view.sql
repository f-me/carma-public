CREATE OR REPLACE VIEW allservicesview AS
                ((        (        (        (        (        (        (        (         SELECT averagecommissionertbl.id,

                                                                                            averagecommissionertbl.type,
                                                                                            averagecommissionertbl.contractor_partner AS towdealer_partner,
                                                                                            averagecommissionertbl.suburbanmilage,
                                                                                            NULL::text AS providedfor,
                                                                                            NULL::timestamp with time zone AS repairenddate,
                                                                                            NULL::int4 AS techtype,
                                                                                            NULL::int4 AS towtype,
                                                                                            averagecommissionertbl.contractor_address AS towaddress_address,
                                                                                            averagecommissionertbl.contractor_partnerid AS towdealer_partnerid,
                                                                                            averagecommissionertbl.assignedto,
                                                                                            averagecommissionertbl.parentid
                                                                                           FROM averagecommissionertbl
                                                                                UNION ALL
                                                                                         SELECT banktbl.id,

                                                                                            banktbl.type,
                                                                                            banktbl.contractor_partner AS towdealer_partner,
                                                                                            banktbl.suburbanmilage,
                                                                                            NULL::text AS providedfor,
                                                                                            NULL::timestamp with time zone AS repairenddate,
                                                                                            NULL::int4 AS techtype,
                                                                                            NULL::int4 AS towtype,
                                                                                            NULL::text AS towaddress_address,
                                                                                            banktbl.contractor_partnerid AS towdealer_partnerid,
                                                                                            banktbl.assignedto,
                                                                                            banktbl.parentid
                                                                                           FROM banktbl)
                                                                        UNION ALL
                                                                                 SELECT consultationtbl.id,

                                                                                    consultationtbl.type,
                                                                                    consultationtbl.contractor_partner AS towdealer_partner,
                                                                                    consultationtbl.suburbanmilage,
                                                                                    NULL::text AS providedfor,
                                                                                    NULL::timestamp with time zone AS repairenddate,
                                                                                    NULL::int4 AS techtype,
                                                                                    NULL::int4 AS towtype,
                                                                                    NULL::text AS towaddress_address,
                                                                                    consultationtbl.contractor_partnerid AS towdealer_partnerid,
                                                                                    consultationtbl.assignedto,
                                                                                    consultationtbl.parentid
                                                                                   FROM consultationtbl)
                                                        UNION ALL
                                                                 SELECT informationtbl.id,

                                                                    informationtbl.type,
                                                                    informationtbl.contractor_partner AS towdealer_partner,
                                                                    informationtbl.suburbanmilage,
                                                                    NULL::text AS providedfor,
                                                                    NULL::timestamp with time zone AS repairenddate,
                                                                    NULL::int4 AS techtype,
                                                                    NULL::int4 AS towtype,
                                                                    informationtbl.contractor_address AS towaddress_address,
                                                                    informationtbl.contractor_partnerid AS towdealer_partnerid,
                                                                    informationtbl.assignedto,
                                                                    informationtbl.parentid
                                                                   FROM informationtbl)
                                                UNION ALL
                                                         SELECT deliverpartstbl.id,

                                                            deliverpartstbl.type,
                                                            deliverpartstbl.contractor_partner AS towdealer_partner,
                                                            deliverpartstbl.suburbanmilage,
                                                            NULL::text AS providedfor,
                                                            NULL::timestamp with time zone AS repairenddate,
                                                            NULL::int4 AS techtype,
                                                            NULL::int4 AS towtype,
                                                            deliverpartstbl.contractor_address AS towaddress_address,
                                                            deliverpartstbl.contractor_partnerid AS towdealer_partnerid,
                                                            deliverpartstbl.assignedto,
                                                            deliverpartstbl.parentid
                                                           FROM deliverpartstbl)
                                        UNION ALL
                                                 SELECT hoteltbl.id,

                                                    hoteltbl.type,
                                                    hoteltbl.contractor_partner AS towdealer_partner,
                                                    hoteltbl.suburbanmilage,
                                                    hoteltbl.providedfor,
                                                    NULL::timestamp with time zone AS repairenddate,
                                                    NULL::int4 AS techtype,
                                                    NULL::int4 AS towtype,
                                                    hoteltbl.contractor_address AS towaddress_address,
                                                    hoteltbl.contractor_partnerid AS towdealer_partnerid,
                                                    hoteltbl.assignedto,
                                                    hoteltbl.parentid
                                                   FROM hoteltbl)
                        UNION ALL
                                 SELECT sobertbl.id,

                                    sobertbl.type,
                                    sobertbl.contractor_partner AS towdealer_partner,
                                    sobertbl.suburbanmilage,
                                    NULL::text AS providedfor,
                                    NULL::timestamp with time zone AS repairenddate,
                                    NULL::int4 AS techtype,
                                    NULL::int4 AS towtype,
                                    sobertbl.contractor_address AS towaddress_address,
                                    sobertbl.contractor_partnerid AS towdealer_partnerid,
                                    sobertbl.assignedto,
                                    sobertbl.parentid
                                   FROM sobertbl)
                UNION ALL
                         SELECT renttbl.id,

                            renttbl.type,
                            renttbl.towdealer_partner,
                            NULL::text AS suburbanmilage,
                            renttbl.providedfor,
                            NULL::timestamp with time zone AS repairenddate,
                            NULL::int4 AS techtype,
                            NULL::int4 AS towtype,
                            NULL::text AS towaddress_address,
                            renttbl.towdealer_partnerid,
                            renttbl.assignedto,
                            renttbl.parentid
                           FROM renttbl)
        UNION ALL
                 SELECT towagetbl.id,

                    towagetbl.type,
                    towagetbl.towdealer_partner,
                    towagetbl.suburbanmilage,
                    NULL::text AS providedfor,
                    towagetbl.repairenddate,
                    NULL::int4 AS techtype,
                    towagetbl.towtype,
                    towagetbl.towaddress_address,
                    towagetbl.towdealer_partnerid,
                    towagetbl.assignedto,
                    towagetbl.parentid
                   FROM towagetbl)
UNION ALL
         SELECT techtbl.id,

            techtbl.type,
            NULL::text AS towdealer_partner,
            techtbl.suburbanmilage,
            NULL::text AS providedfor,
            NULL::timestamp with time zone AS repairenddate,
            techtbl.techtype,
            NULL::int4 AS towtype,
            NULL::text AS towaddress_address,
            NULL::int4 AS towdealer_partnerid,
            techtbl.assignedto,
            techtbl.parentid
           FROM techtbl)
UNION ALL
SELECT taxitbl.id,
       taxitbl.type,
       taxitbl.contractor_partner AS towdealer_partner,
       NULL::text AS suburbanmilage,
       NULL::text AS providedfor,
       NULL::TIMESTAMP WITH time ZONE AS repairenddate,
       NULL::integer AS techtype,
       NULL::integer AS towtype,
       taxitbl.taxito_address AS towaddress_address,
       taxitbl.contractor_partnerid AS towdealer_partnerid,
       taxitbl.assignedto,
       taxitbl.parentid
FROM taxitbl;
