DELETE FROM usermetatbl
  WHERE ctid IN
    (SELECT max(ctid) FROM usermetatbl
      WHERE id IN
        (SELECT id FROM usermetatbl GROUP BY id HAVING count(*) > 1)
      GROUP BY id);

ALTER TABLE usermetatbl ADD PRIMARY KEY (id);
