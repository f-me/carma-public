DROP VIEW "Услуги";
DROP VIEW allservicesview;


ALTER TABLE servicetbl
ALTER COLUMN contractor_partnerId
TYPE int4 USING(nullif(substring(contractor_partnerId, ':(.*)'), '')::int4);
UPDATE servicetbl SET contractor_partnerId = NULL
WHERE NOT EXISTS (SELECT 1 FROM partnertbl p WHERE p.id = contractor_partnerId);
ALTER TABLE servicetbl
ADD CONSTRAINT "servicetbl_contractor_partnerId_fkey"
FOREIGN KEY (contractor_partnerId) REFERENCES partnertbl (id);


ALTER TABLE towagetbl
ALTER COLUMN towDealer_partnerId
TYPE int4 USING(nullif(substring(towDealer_partnerId, ':(.*)'), '')::int4);
UPDATE towagetbl SET towDealer_partnerId = NULL
WHERE NOT EXISTS (SELECT 1 FROM partnertbl p WHERE p.id = towDealer_partnerId);
ALTER TABLE towagetbl
ADD CONSTRAINT "towagetbl_towDealer_partnerId_fkey"
FOREIGN KEY (towDealer_partnerId) REFERENCES partnertbl (id);


ALTER TABLE renttbl
ALTER COLUMN towDealer_partnerId
TYPE int4 USING(nullif(substring(towDealer_partnerId, ':(.*)'), '')::int4);
UPDATE renttbl SET towDealer_partnerId = NULL
WHERE NOT EXISTS (SELECT 1 FROM partnertbl p WHERE p.id = towDealer_partnerId);
ALTER TABLE renttbl
ADD CONSTRAINT "renttbl_towDealer_partnerId_fkey"
FOREIGN KEY (towDealer_partnerId) REFERENCES partnertbl (id);
