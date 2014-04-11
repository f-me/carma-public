CREATE OR REPLACE FUNCTION array_unique(non_unique_array_vals anyarray)
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

UPDATE usermetatbl SET boPrograms = res.boPrograms::text[] FROM
 (SELECT u.id, array_unique(array_agg("Program".id)) AS boPrograms FROM
  (SELECT id, unnest(boPrograms) AS program FROM usermetatbl WHERE array_length(boPrograms, 1) > 0) u, "SubProgram" s, "Program"
   WHERE u.program = s.value and "Program".id = s.parent GROUP BY u.id) res
WHERE res.id = usermetatbl.id;

DROP FUNCTION IF EXISTS array_unique(anyarray);
