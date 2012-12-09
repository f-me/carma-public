CREATE EXTENSION postgis;

CREATE ROLE carma_search PASSWORD 'md568023aeacae5a76b23b958eb5da1a994' NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO carma_search;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO carma_search;

CREATE ROLE carma_db_sync PASSWORD 'md556d33ece5e1452257fa0a086e7945c0b' NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO carma_db_sync; -- FIXME:

CREATE ROLE carma_geo PASSWORD 'md5a73940ffdfdd8d8b9ecfbfba6cc3e2ab' NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN;

CREATE ROLE carma_action_assignment ENCRYPTED PASSWORD 'md5039cf6a6d8de18b95bd103f64c1dfab9' NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN;
GRANT SELECT, UPDATE ON actiontbl TO carma_action_assignment;

-- Run this after first sync

GRANT SELECT, UPDATE ON partnertbl TO carma_geo;
GRANT SELECT ON partnerMessageTbl TO carma_geo;
GRANT ALL PRIVILEGES ON spatial_ref_sys TO carma_geo;


-- create indices
CREATE INDEX ON calltbl USING hash (callerName_phone1);

CREATE INDEX ON casetbl USING btree (callDate);

CREATE INDEX ON partnertbl USING hash (isActive);
CREATE INDEX ON partnertbl USING hash (isDealer) where isActive = true;
CREATE INDEX ON partnertbl USING hash (city) where isActive = true;

CREATE INDEX ON actiontbl USING hash (closed);
CREATE INDEX ON actiontbl USING hash (targetGroup);
CREATE INDEX ON actiontbl USING hash (caseId);
CREATE INDEX ON actiontbl USING btree (duetime) where closed = false;
