revoke all privileges on all tables in schema public from carma_action_assignment;
DROP ROLE IF EXISTS carma_action_assignment;

revoke all privileges on all tables in schema public from carma_genser_svc;
DROP ROLE IF EXISTS carma_genser_svc;

revoke all privileges on all tables in schema public from analyst;
DROP ROLE IF EXISTS analyst;

revoke all privileges on all tables in schema public from carma_search;
revoke all privileges on all sequences in schema public from carma_search;
alter default privileges revoke all privileges on tables from carma_search;
alter default privileges revoke all privileges on sequences from carma_search;
REASSIGN OWNED BY carma_search TO carma;
DROP OWNED BY carma_search;
DROP ROLE IF EXISTS carma_search;
