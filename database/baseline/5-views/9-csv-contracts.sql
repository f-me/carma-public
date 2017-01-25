DROP VIEW IF EXISTS "Contract_csv";
CREATE VIEW "Contract_csv" AS
SELECT "Contract".id AS idExternal,
        CASE "Contract".isactive
            WHEN true THEN '+'::text
            ELSE '-'::text
        END AS isactiveExternal,
    "Contract".name AS nameExternal,
    "Contract".email AS emailExternal,
    "Contract".vin AS vinExternal,
    "Contract".cardnumber AS cardnumberExternal,
    "Contract".codeword AS codewordExternal,
    "Contract".phone AS phoneExternal,
    "Contract".platenum AS platenumExternal,
    to_char("Contract".validsince, 'DD.MM.YYYY') AS validsinceExternal,
    to_char("Contract".validuntil, 'DD.MM.YYYY') AS validuntilExternal,
    "Contract".startmileage AS startmileageExternal,
    "CarMake".label AS makeExternal,
    "CarModel".label AS modelExternal,
    "Contract".makeyear::Integer makeyearExternal,
    "CarClass".label AS carclassExternal,
    "Contract".color AS colorExternal,
    "Transmission".label AS transmissionExternal,
    "Contract".enginevolume AS enginevolumeExternal,
    "Engine".label AS enginetypeExternal,
    to_char("Contract".buydate, 'DD.MM.YYYY') AS buydateExternal,
    p2.name AS sellerExternal,
    p1.name AS lastcheckdealerExternal,
    "Contract".checkperiod AS checkperiodExternal,
    "CheckType".label AS checktypeExternal,
    "Contract".ordernumber AS ordernumberExternal,
    "Contract".managername AS managernameExternal,
    "Contract".comment AS commentExternal,
    "LegalForm".label AS legalformExternal,
    to_char("Contract".ctime, 'DD.MM.YYYY HH24:MI') AS ctimeExternal,
    usermetatbl.realname AS committerExternal,
        CASE "Contract".dixi
            WHEN true THEN '+'::text
            ELSE '-'::text
        END AS dixiExternal,
    p2.code as dealercodeExternal,
    "ContractRegistrationReason".label AS registrationreasonExternal,
    "Contract".priceinorder AS priceinorderExternal,
    "Contract".id,
    "Contract".name,
    "Contract".email,
    "Contract".vin,
    "Contract".cardnumber,
    "Contract".codeword,
    "Contract".phone,
    "Contract".platenum,
    "Contract".validsince,
    "Contract".validuntil,
    "Contract".startmileage,
    "Contract".make,
    "Contract".model,
    "Contract".makeyear,
    "Contract".carclass,
    "Contract".color,
    "Contract".transmission,
    "Contract".enginevolume,
    "Contract".enginetype,
    "Contract".buydate,
    "Contract".seller,
    "Contract".lastcheckdealer,
    "Contract".checktype,
    "Contract".ordernumber,
    "Contract".managername,
    "Contract".comment,
    "Contract".subprogram,
    "Contract".legalform,
    "Contract".committer,
    "Contract".dixi,
    "Contract".isactive,
    "Contract".ctime,
    "Contract".checkperiod,
    "Contract".registrationreason,
    "Contract".priceinorder
   FROM "Contract"
   LEFT JOIN "CarClass" ON "Contract".carclass = "CarClass".id
   LEFT JOIN "CheckType" ON "Contract".checktype = "CheckType".id
   LEFT JOIN usermetatbl ON "Contract".committer = usermetatbl.id
   LEFT JOIN "Engine" ON "Contract".enginetype = "Engine".id
   LEFT JOIN partnertbl p1 ON "Contract".lastcheckdealer = p1.id
   LEFT JOIN "LegalForm" ON "Contract".legalform = "LegalForm".id
   LEFT JOIN "CarMake" ON "Contract".make = "CarMake".id
   LEFT JOIN "CarModel" ON "Contract".model = "CarModel".id
   LEFT JOIN partnertbl p2 ON "Contract".seller = p2.id
   LEFT JOIN "SubProgram" ON "Contract".subprogram = "SubProgram".id
   LEFT JOIN "Program" ON "SubProgram".parent = "Program".id
   LEFT JOIN "ProgramType" ON "Program".ptype = "ProgramType".id
   LEFT JOIN "Transmission" ON "Contract".transmission = "Transmission".id
   LEFT JOIN "ContractRegistrationReason" ON "Contract".registrationreason = "ContractRegistrationReason".id;
GRANT SELECT ON "Contract_csv" TO carma_db_sync;
