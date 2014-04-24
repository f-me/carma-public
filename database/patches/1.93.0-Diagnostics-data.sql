INSERT INTO "System" (id, label)
SELECT id, label FROM "Diagnosis1";

SELECT setval(pg_get_serial_sequence('"System"', 'id'), max(id)) from "System";


INSERT INTO "Part" (id, parent, label)
SELECT id, parent, label FROM "Diagnosis2";

SELECT setval(pg_get_serial_sequence('"Part"', 'id'), max(id)) from "Part";


INSERT INTO "Cause" (id, label)
SELECT id, label FROM "Diagnosis3";

SELECT setval(pg_get_serial_sequence('"Cause"', 'id'), max(id)) from "Cause";


INSERT INTO "Suggestion" (id, label)
SELECT id, label FROM "Diagnosis4";

SELECT setval(pg_get_serial_sequence('"Suggestion"', 'id'), max(id)) from "Suggestion";


INSERT INTO "Wazzup" (label, system, part, cause, suggestion)
SELECT d0.label, d1.id, d2.id, d3.id, d4.id
FROM "Diagnosis0" d0,
     "Diagnosis1" d1,
     "Diagnosis2" d2,
     "Diagnosis3" d3,
     "Diagnosis4" d4
WHERE d1val = d1.value
  AND d2val = d2.value
  AND d3val = d3.value
  AND d4val = d4.value;

INSERT INTO "Wazzup" (label)
SELECT label FROM "Diagnosis0" WHERE id = 34 OR id = 54;

SELECT setval(pg_get_serial_sequence('"Wazzup"', 'id'), max(id)) from "Wazzup";
