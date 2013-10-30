UPDATE usermetatbl SET roles=q.rids FROM (
 SELECT s.id, array_agg(r.id) AS rids FROM (
  SELECT DISTINCT id,unnest(roles) as role FROM usermetatbl) s,
 "Role" r
 WHERE s.role=r.value GROUP BY s.id) q
WHERE usermetatbl.id=q.id;
