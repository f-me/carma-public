WITH remap AS
(SELECT old.value, p.id AS pid, sp.id AS sid
 FROM programtbl old, "Program" p, "SubProgram" sp
 WHERE sp.parent=p.id AND sp.value=old.value)
UPDATE calltbl
SET program = remap.pid::text, subprogram = sid
FROM remap WHERE
program = remap.value
AND
program NOT IN (SELECT id::text FROM "Program")
AND
program IN (SELECT VALUE FROM programtbl);
