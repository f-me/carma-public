DROP   VIEW IF EXISTS partnercancelview;
CREATE VIEW partnercancelview AS
  SELECT pc.*
       , p.name as partner
       , substring(pc.caseid   , ':(.*)') as case
       , substring(pc.serviceid, ':(.*)') as service
       , substring(pc.serviceid, '(.*):') as type
       , c.city
       , r.label as region
  FROM partnercanceltbl pc
  LEFT JOIN partnertbl       p
  ON p.id::text = substring(pc.partnerid, ':(.*)')
  LEFT JOIN casetbl          c
  ON c.id::text = substring(pc.caseid   , ':(.*)')
  LEFT OUTER JOIN "City" city
  ON c.city = city.value
  LEFT OUTER JOIN "Region" r
  ON city.id = ANY(r.cities);

GRANT SELECT ON partnercancelview TO carma_db_sync;
