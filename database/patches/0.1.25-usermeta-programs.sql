WITH newprograms AS
(WITH oldids AS (SELECT id as usermetaid, unnest(programs) as pid FROM usermetatbl) 
   SELECT oldids.usermetaid, array_agg(sp.id) as pids
     FROM oldids, programtbl p, "SubProgram" sp
    WHERE oldids.pid = p.id::text
      AND sp.value = p.value
 GROUP BY usermetaid)
UPDATE usermetatbl
   SET programs = (SELECT pids
                     FROM newprograms
                    WHERE newprograms.usermetaid = usermetatbl.id)
 WHERE id IN (SELECT usermetaid FROM newprograms);
