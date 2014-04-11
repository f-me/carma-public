-- Merge opeldo & opel programs
UPDATE casetbl SET program = 'opel' WHERE program = 'opeldo';

-- Merge fordPlus & ford
UPDATE casetbl SET program = 'ford' WHERE program = 'fordPlus';

-- Migrate cases with known programs
WITH remap AS
(SELECT old.value, p.id AS pid, sp.id AS sid
 FROM programtbl old, "Program" p, "SubProgram" sp
 WHERE sp.parent=p.id AND sp.value=old.value)
UPDATE casetbl
SET program = remap.pid::text, subprogram = sid
FROM remap WHERE program = remap.value;
