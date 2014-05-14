ALTER TABLE partnertbl ADD COLUMN taxScheme_tmp int4;

UPDATE partnertbl SET taxScheme_tmp = 1 WHERE taxScheme = '1';
UPDATE partnertbl SET taxScheme_tmp = 2 WHERE taxScheme = '2';
UPDATE partnertbl SET taxScheme_tmp = 3 WHERE taxScheme = '3';

ALTER TABLE partnertbl DROP COLUMN taxScheme;
ALTER TABLE partnertbl ADD COLUMN taxScheme int4 REFERENCES "TaxScheme";
UPDATE partnertbl SET taxScheme = taxScheme_tmp WHERE taxScheme_tmp IS NOT NULL;
ALTER TABLE partnertbl DROP COLUMN taxScheme_tmp;
