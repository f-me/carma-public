
create table "AvarcomTask"
  ( id serial primary key
  , label text unique not null
  , isActive bool not null default true
  );

insert into "AvarcomTask" (label) values
  ('Составить акт осмотра')
, ('Сделать фотографии')
, ('Помочь заполнить заявление')
;

insert into "Dictionary" (id, name, description, parent, majorFields) values
(58, 'AvarcomTask'
, 'Задачи аварийного комиссара'
, array[]::int4[], array['id', 'label'])
;

grant all on "AvarcomTask" to carma_db_sync;
grant all on "AvarcomTask_id_seq" to carma_db_sync;

insert into "FieldPermission" (role, model, field, r, w) values
 (7, 'AvarcomTask', 'id', 't', 'f')
,(6, 'AvarcomTask', 'id', 't', 't')
,(7, 'AvarcomTask', 'label', 't', 'f')
,(6, 'AvarcomTask', 'label', 't', 't')
,(7, 'AvarcomTask', 'isActive', 't', 'f')
,(6, 'AvarcomTask', 'isActive', 't', 't')
;
