-- Never run, preserved for reference
GRANT SELECT ON ALL TABLES IN SCHEMA public TO carma_search;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO carma_search;

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO carma_db_sync; -- FIXME:

GRANT SELECT, UPDATE ON partnertbl TO carma_geo;
GRANT ALL PRIVILEGES ON spatial_ref_sys TO carma_geo;
