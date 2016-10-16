
create table "AvarcomTask"
  ( id int primary key
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
