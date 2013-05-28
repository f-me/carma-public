CREATE VIEW partnercancelview AS
  SELECT pc.*, p.name as partner, substring(pc.caseid, ':(.*)') as case
  FROM      partnercanceltbl pc
  LEFT JOIN partnertbl       p
  ON p.id::text = substring(pc.partnerid, ':(.*)');

GRANT SELECT ON partnercancelview TO carma_db_sync;
