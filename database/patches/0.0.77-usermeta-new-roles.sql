CREATE TEMPORARY TABLE usermetatbl_tmp
   ( id integer
   , roles text[]
   );

INSERT INTO usermetatbl_tmp (SELECT id, roles FROM usermetatbl);

UPDATE usermetatbl SET roles = null;

UPDATE usermetatbl u SET roles=u.roles || array['26','14','1']
FROM usermetatbl_tmp t WHERE '12'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['8', '27','14','1']
FROM usermetatbl_tmp t WHERE '13'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['22','12','14','1']
FROM usermetatbl_tmp t WHERE '11'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['25', '14', '1']
FROM usermetatbl_tmp t WHERE '15'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['24','14','1']
FROM usermetatbl_tmp t WHERE '5'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['23','14','1']
FROM usermetatbl_tmp t WHERE '4'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','20','33']
FROM usermetatbl_tmp t WHERE '6'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['9','1']
FROM usermetatbl_tmp t WHERE '9'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['10','1']
FROM usermetatbl_tmp t WHERE '8'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['33','1']
FROM usermetatbl_tmp t WHERE '3'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['4','1']
FROM usermetatbl_tmp t WHERE '16'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['6','1']
FROM usermetatbl_tmp t WHERE '17'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['19']
FROM usermetatbl_tmp t WHERE '10'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['19','1']
FROM usermetatbl_tmp t WHERE '22'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['20', '1']
FROM usermetatbl_tmp t WHERE '21'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['28','29','14','3','1']
FROM usermetatbl_tmp t WHERE '7'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['30','4','1']
FROM usermetatbl_tmp t WHERE '19'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['13','14','1']
FROM usermetatbl_tmp t WHERE '18'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['31','14','1']
FROM usermetatbl_tmp t WHERE '20'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['25','14','1']
FROM usermetatbl_tmp t WHERE '15'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['32', '1']
FROM usermetatbl_tmp t WHERE '24'=ANY(t.roles)
AND t.id=u.id;

UPDATE usermetatbl u SET roles=u.roles || array['15', '1']
FROM usermetatbl_tmp t WHERE '14'=ANY(t.roles)
AND t.id=u.id;

DROP FUNCTION IF EXISTS array_unique(anyarray);

CREATE FUNCTION array_unique(non_unique_array_vals anyarray)
RETURNS TABLE (unique_array anyarray) AS $BODY$
    BEGIN
      RETURN QUERY SELECT
        ARRAY_AGG(unique_vals.val)
      FROM
        (
  SELECT
  UNNEST(non_unique_array_vals) AS val
  GROUP BY val) AS unique_vals;
    END;
  $BODY$
LANGUAGE plpgsql;

UPDATE usermetatbl SET roles = array_unique(roles);

DROP FUNCTION IF EXISTS array_unique(anyarray);
