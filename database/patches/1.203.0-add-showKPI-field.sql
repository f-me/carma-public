BEGIN;

ALTER TABLE usermetatbl ADD COLUMN showKPI boolean DEFAULT 'f';
UPDATE usermetatbl
  SET showKPI = 't'
  WHERE roles && '{22,23,24,25,26,27,28,29,30,31,41,2}';

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES (4,  'Usermeta', 'showKPI', 't', 't')
      ,(12, 'Usermeta', 'showKPI', 't', 't')
      ,(5,  'Usermeta', 'showKPI', 't', 'f');

END;
