UPDATE "SubProgram" SET value='rnbase' WHERE id=56;

CREATE TEMPORARY TABLE contract_tmp AS (SELECT * FROM contracttbl WHERE 'f');

-- Transfer selected contracts to temporary table
INSERT INTO contract_tmp
SELECT c.* FROM contracttbl c, programtbl p
WHERE p.value=ANY(array['alarmass','ruslan','vwCarePoint','rnbase','nz'])
AND p.id::text=c.program;

-- Set new subprogram
UPDATE contract_tmp c
SET program = s.id::text FROM "SubProgram" s, programtbl p
WHERE p.id::text = c.program
AND p.value = s.value;

-- Migrate dictionary fields from values to ids. Clean bad values.
UPDATE contract_tmp SET carmake = null WHERE NOT carmake IN
(SELECT value FROM "CarMake");
UPDATE contract_tmp c
SET carmake = m.id::text FROM "CarMake" m
WHERE m.value = c.carmake;

UPDATE contract_tmp SET carmodel = null WHERE NOT carmodel IN
(SELECT value FROM "CarModel");
UPDATE contract_tmp c
SET carmodel = m.id::text FROM "CarModel" m
WHERE m.value = c.carmodel;

UPDATE contract_tmp SET carcolor = null WHERE NOT carcolor IN
(SELECT value FROM "Colors");
UPDATE contract_tmp c
SET carcolor = l.id::text FROM "Colors" l
WHERE l.value = c.carcolor;

UPDATE contract_tmp SET cartransmission = '1' WHERE cartransmission='auto';
UPDATE contract_tmp SET cartransmission = '2' WHERE cartransmission='mech';
UPDATE contract_tmp SET cartransmission = '3' WHERE cartransmission='robot';
UPDATE contract_tmp SET cartransmission = null WHERE NOT cartransmission IN
(SELECT id::text FROM "Transmission");

UPDATE contract_tmp SET carengine = '1' WHERE carengine='ptrl';
UPDATE contract_tmp SET carengine = '2' WHERE carengine='dis';
UPDATE contract_tmp SET carengine = null WHERE NOT carengine IN
(SELECT id::text FROM "Engine");

UPDATE contract_tmp SET techtype = '1' WHERE techtype = 'to1';
UPDATE contract_tmp SET techtype = '2' WHERE techtype = 'to2';
UPDATE contract_tmp SET techtype = '3' WHERE techtype = 'to3';
UPDATE contract_tmp SET techtype = null WHERE NOT techtype IN
(SELECT id::text FROM "CheckType");

UPDATE contract_tmp SET carseller = null WHERE NOT carseller IN
(SELECT id::text FROM partnertbl);

-- Use usermeta id, not Snap user id in new contracts
UPDATE contract_tmp c
SET owner = u.id::text FROM usermetatbl u
WHERE c.owner = u.uid::text;

-- Assign top committer for a program as the owner of orphan contracts
-- in that program
WITH owners AS
(SELECT count(*), owner, program FROM contract_tmp
 WHERE owner IS NOT NULL
 GROUP BY owner,program ORDER BY count DESC)
UPDATE contract_tmp c SET owner =
(SELECT owner FROM owners WHERE program = c.program LIMIT 1)
WHERE owner IS null;

-- Sanify values
UPDATE contract_tmp SET isactive='t' WHERE isactive IS null;
UPDATE contract_tmp SET carmakeyear = null WHERE carmakeyear > 2014;

-- Do not import empty contracts
DELETE FROM contract_tmp
WHERE coalesce(carvin, '')=''
AND coalesce(cardnumber,'')=''
AND coalesce(cardowner,'')='';

-- Transfer to live table
INSERT INTO "Contract"
      (name,
       vin,
       cardnumber,
       platenum,
       validsince,
       validuntil,
       startmileage,
       make,
       model,
       makeyear,
       color,
       transmission,
       enginetype,
       buydate,
       seller,
       checktype,
       ordernumber,
       managername,
       committer,
       isactive,
       checkperiod,
       subprogram,
       ctime)
      SELECT
       cardowner,
       carvin,
       cardnumber,
       carplatenum,
       (contractvalidfromdate + '4 hours')::date,
       (contractvaliduntildate + '4 hours')::date,
       milageTO::int,
       carmake::int,
       carmodel::int,
       carmakeyear::int,
       carcolor,
       cartransmission::int,
       carengine::int,
       (carbuydate + '4 hours')::date,
       carseller::int,
       techtype::int,
       ordernumber,
       manager,
       owner::int,
       isactive,
       carcheckperiod,
       program::int,
       ctime
      FROM contract_tmp;
