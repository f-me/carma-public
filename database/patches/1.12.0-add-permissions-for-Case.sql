insert into "FieldPermission" (role, model, field, r, w)
  select role, 'Case', field, r, w
    from "FieldPermission" p1
    where model = 'case'
      and not exists
        (select 1 from "FieldPermission" p2
          where p2.role  = p1.role
            and p2.field = p1.field
            and p2.model = 'Case')
