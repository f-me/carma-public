DROP   VIEW IF EXISTS partnercancelview;
CREATE VIEW partnercancelview AS
  SELECT pc.*
       , p.name as partner
       , substring(pc.caseid   , ':(.*)') as case
       , substring(pc.serviceid, ':(.*)') as service
       , substring(pc.serviceid, '(.*):') as serviceName
       , c.city
  FROM      partnercanceltbl pc
  LEFT JOIN partnertbl       p
  ON p.id::text = substring(pc.partnerid, ':(.*)')
  LEFT JOIN casetbl          c
  ON c.id::text = substring(pc.caseid   , ':(.*)');

GRANT SELECT ON partnercancelview TO carma_db_sync;
