BEGIN;
  INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (1, 'Usermeta', 'currentStateCTime', '1', '0');
END;
