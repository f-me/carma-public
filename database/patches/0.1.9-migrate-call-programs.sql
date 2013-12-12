-- Merge opeldo & opel programs
UPDATE calltbl SET program = 'opel' WHERE program = 'opeldo';

-- Migrate calls with known programs
WITH remap AS 
(SELECT old.value, p.id AS pid, sp.id AS sid
 FROM programtbl old, "Program" p, "SubProgram" sp
 WHERE sp.parent=p.id AND sp.value=old.value)
UPDATE calltbl
SET program = (SELECT pid::text FROM remap WHERE remap.value=calltbl.program),
 subprogram = (SELECT sid FROM remap WHERE remap.value=calltbl.program)
FROM remap WHERE program IN (SELECT value FROM remap);
