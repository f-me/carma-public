CREATE FUNCTION pg_temp.clean_users() RETURNS int AS $$
BEGIN
IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'carma_action_assignment') THEN
  revoke all privileges on all tables in schema public from carma_action_assignment;
  DROP ROLE carma_action_assignment;
END IF;

IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'carma_genser_svc') THEN
  revoke all privileges on all tables in schema public from carma_genser_svc;
  DROP ROLE carma_genser_svc;
END IF;

IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'analyst') THEN
  revoke all privileges on all tables in schema public from analyst;
  DROP ROLE analyst;
END IF;

IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'carma_search') THEN
  revoke all privileges on all tables in schema public from carma_search;
  revoke all privileges on all sequences in schema public from carma_search;
  alter default privileges revoke all privileges on tables from carma_search;
  alter default privileges revoke all privileges on sequences from carma_search;
  REASSIGN OWNED BY carma_search TO carma;
  DROP OWNED BY carma_search;
  DROP ROLE IF EXISTS carma_search;
END IF;

RETURN 0;
END;
$$ LANGUAGE plpgsql;

SELECT pg_temp.clean_users();
