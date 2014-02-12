insert into "FieldPermission" (role, model, field, r, w)
(select role, 'Call' as model, field, r, w
        from "FieldPermission"
        where model = 'call');
