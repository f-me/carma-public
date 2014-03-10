DROP VIEW servicesview;

ALTER TABLE casetbl ADD FOREIGN KEY (subprogram) REFERENCES "SubProgram";
ALTER TABLE calltbl ADD FOREIGN KEY (subprogram) REFERENCES "SubProgram";

ALTER TABLE casetbl ADD COLUMN program_tmp int4;
UPDATE casetbl SET program = null WHERE
program NOT IN (SELECT id::text FROM "Program");
UPDATE casetbl SET program_tmp = program::int4;
ALTER TABLE casetbl DROP COLUMN program;
ALTER TABLE casetbl ADD COLUMN program int4 REFERENCES "Program";
ALTER TABLE casetbl DROP COLUMN program_tmp;

ALTER TABLE calltbl ADD COLUMN program_tmp int4;
UPDATE calltbl SET program = null WHERE
program NOT IN (SELECT id::text FROM "Program");
UPDATE calltbl SET program_tmp = program::int4;
ALTER TABLE calltbl DROP COLUMN program;
ALTER TABLE calltbl ADD COLUMN program int4 REFERENCES "Program";
ALTER TABLE calltbl DROP COLUMN program_tmp;
