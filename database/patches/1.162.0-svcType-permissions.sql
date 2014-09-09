delete from "FieldPermission" where model='Service' and field='type';
insert into "FieldPermission" (role, model, field, r, w)
select role, model, 'type', 't', 'f' from "FieldPermission" where field='createTime';
