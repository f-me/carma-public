insert into "FieldPermission" (role, model, field, r, w)
(select role, 'Case' as model, field, r, w
        from "FieldPermission"
        where model = 'case');
