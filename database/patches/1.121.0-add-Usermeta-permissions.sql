BEGIN;
  INSERT INTO "FieldPermission" (role, model, field, r, w)
  SELECT role, 'Usermeta' as model, field, r, w
  FROM "FieldPermission"
  WHERE model = 'usermeta';

  -- 12 - supervisor
  INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (12, 'Usermeta', 'bocities', 't', 't'),
         (12, 'Usermeta', 'boprograms', 't', 't'),
         (12, 'Usermeta', 'businessRole', 't', 't'),
         (1, 'Usermeta',  'currentState', 't', 'f'),
         (1, 'Usermeta',  'delayedState', 't', 't');
COMMIT;
