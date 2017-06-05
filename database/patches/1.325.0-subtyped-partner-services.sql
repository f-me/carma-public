BEGIN;

ALTER TABLE partnertbl ADD COLUMN services_backup json NOT NULL DEFAULT '[]'::json;
UPDATE partnertbl SET services_backup = services;

-- Generate "subtypes" field for all known/active service subtypes,
-- re-using priorities from previously non-subtyped service
CREATE OR REPLACE FUNCTION pg_temp.bike_towage_subtypes(int, int, int) RETURNS json AS $$
    SELECT json_agg(s.r) FROM
      (SELECT row_to_json(t) AS r FROM
          (WITH types AS (SELECT unnest AS typ FROM unnest('{1, 2, 3, 4, 5}'::int[]))
            (SELECT typ AS subtype, $1 AS priority1, $2 AS priority2, $3 AS priority3 FROM types)) t) s;
    $$
LANGUAGE SQL
IMMUTABLE;

CREATE OR REPLACE FUNCTION pg_temp.towage_subtypes(int, int, int) RETURNS json AS $$
    SELECT json_agg(s.r) FROM
      (SELECT row_to_json(t) AS r FROM
          (WITH types AS (SELECT unnest AS typ FROM unnest('{1, 2, 3, 4, 5, 6, 7}'::int[]))
            (SELECT typ AS subtype, $1 AS priority1, $2 AS priority2, $3 AS priority3 FROM types)) t) s;
    $$
LANGUAGE SQL
IMMUTABLE;

CREATE OR REPLACE FUNCTION pg_temp.tech_subtypes(int, int, int) RETURNS json AS $$
    SELECT json_agg(s.r) FROM
      (SELECT row_to_json(t) AS r FROM
          (WITH types AS (SELECT unnest AS typ FROM unnest('{27, 28, 29, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41}'::int[]))
            (SELECT typ AS subtype, $1 AS priority1, $2 AS priority2, $3 AS priority3 FROM types)) t) s;
    $$
LANGUAGE SQL
IMMUTABLE;

CREATE OR REPLACE FUNCTION pg_temp.add_subtypes(json) RETURNS json AS $$
  SELECT
      CASE
      WHEN $1->>'type' = '1' THEN regexp_replace($1::text, '}$', ', "subtypes": ' || pg_temp.tech_subtypes(($1->>'priority1')::int, ($1->>'priority2')::int, ($1->>'priority3')::int)::text || '}')::json
      WHEN $1->>'type' = '2' THEN regexp_replace($1::text, '}$', ', "subtypes": ' || pg_temp.towage_subtypes(($1->>'priority1')::int, ($1->>'priority2')::int, ($1->>'priority3')::int)::text || '}')::json
      WHEN $1->>'type' = '19' THEN regexp_replace($1::text, '}$', ', "subtypes": ' || pg_temp.bike_towage_subtypes(($1->>'priority1')::int, ($1->>'priority2')::int, ($1->>'priority3')::int)::text || '}')::json
      ELSE $1
      END;
  $$
LANGUAGE SQL
IMMUTABLE;


-- An attempt to do it on structural level: decompose JSON into sets
-- of top-level key/value pairs, do a UNION with extra pairs. There
-- doesn't seem to be a way to build JSON back from pairs. jsonb and
-- PG >= 9.5 (||) would helps here.

-- WITH numbered_partner_services AS
-- (WITH partner_services AS
-- (SELECT id AS partner_id, json_array_elements(services::json) AS service FROM partnertbl WHERE id IN (6, 7)) -- just a few partners for test
-- SELECT partner_id, row_number() OVER () AS global_number, service
-- FROM partner_services)
-- SELECT partner_id, global_number, json_each(service) FROM numbered_partner_services
-- UNION ALL
-- SELECT partner_id, global_number, ROW('subtypes', pg_temp.tech_subtypes((service->>'priority1')::int, (service->>'priority2')::int, (service->>'priority3')::int)) FROM numbered_partner_services WHERE service->>'type' = '1'
-- UNION ALL
-- SELECT partner_id, global_number, ROW('subtypes', pg_temp.towage_subtypes((service->>'priority1')::int, (service->>'priority2')::int, (service->>'priority3')::int)) FROM numbered_partner_services WHERE service->>'type' = '2'
-- UNION ALL
-- SELECT partner_id, global_number, ROW('subtypes', pg_temp.bike_towage_subtypes((service->>'priority1')::int, (service->>'priority2')::int, (service->>'priority3')::int)) FROM numbered_partner_services WHERE service->>'type' = '19';

UPDATE partnertbl SET services = q.new_services FROM
(WITH numbered_partner_services AS
(WITH partner_services AS
(SELECT id AS partner_id, json_array_elements(services::json) AS service FROM partnertbl)
SELECT partner_id, row_number() OVER () AS global_number, service
FROM partner_services)
SELECT partner_id, json_agg(processed_service)::json AS new_services FROM
(SELECT partner_id, global_number, pg_temp.add_subtypes(service) AS processed_service FROM numbered_partner_services) s GROUP BY s.partner_id) q
WHERE q.partner_id = partnertbl.id;

COMMIT;
