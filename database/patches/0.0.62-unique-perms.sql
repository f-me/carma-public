delete from "FieldPermission"
  where id not in
    (select min(id) from "FieldPermission" group by role, model, field);
ALTER TABLE "FieldPermission"
  ADD CONSTRAINT FieldPermission_rmf_unique UNIQUE (role, model, field);
