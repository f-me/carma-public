DELETE FROM attachmenttbl
  WHERE ctid IN
    (SELECT max(ctid) FROM attachmenttbl GROUP BY id HAVING count(*) > 1);

ALTER TABLE attachmenttbl
ADD CONSTRAINT attachmenttbl_pkey PRIMARY KEY (id);

DELETE FROM actiontbl
  WHERE ctid IN
    (SELECT max(ctid) FROM actiontbl GROUP BY id HAVING count(*) > 1);

ALTER TABLE actiontbl
ADD CONSTRAINT actiontbl_pkey PRIMARY KEY (id);

DELETE FROM partner_servicetbl
  WHERE ctid IN
    (SELECT max(ctid) FROM partner_servicetbl GROUP BY id HAVING count(*) > 1);

ALTER TABLE partner_servicetbl
ADD CONSTRAINT partner_servicetbl_pkey PRIMARY KEY (id);

DELETE FROM partnercanceltbl
  WHERE ctid IN
    (SELECT max(ctid) FROM partnercanceltbl GROUP BY id HAVING count(*) > 1);

ALTER TABLE partnercanceltbl
ADD CONSTRAINT partnercanceltbl_pkey PRIMARY KEY (id);
