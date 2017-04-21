
delete from "FieldPermission" where model = 'DiagHistory' or model = 'DiagSlide';
drop table if exists "DiagHistory";
drop table if exists "DiagSlide";



create table "DiagSlide"
  ( id serial primary key
  , ctime timestamptz not null default now()
  , header text not null
  , body text not null default ''
  , resources json not null default '[]'::json
  , answers json not null default '[]'::json
  , actions json not null default '[]'::json
  , isRoot bool not null default false
  );


insert into "DiagSlide"
  (header, body, isRoot) values
  ('Новый вопрос', '?', true);


grant all on "DiagSlide" to carma_db_sync;
grant all on "DiagSlide_id_seq" to carma_db_sync;


insert into "FieldPermission" (role, model, field, r, w) values
  (1, 'DiagSlide', 'header', true, true)
, (1, 'DiagSlide', 'body', true, true)
, (1, 'DiagSlide', 'resources', true, true)
, (1, 'DiagSlide', 'answers', true, true)
, (1, 'DiagSlide', 'isRoot', true, true)
;



create table "DiagHistory"
  ( id serial primary key
  , ctime timestamptz not null default now()
  , caseId int not null references casetbl(id)
  , slideId int not null references "DiagSlide"(id)
  , createdBy int not null references usermetatbl(id)
  , answerIx int
  , answeredBy int references usermetatbl(id)
  , answerTime timestamptz
  , deprecatedBy int references "DiagHistory"(id)
  );


grant all on "DiagHistory" to carma_db_sync;
grant all on "DiagHistory_id_seq" to carma_db_sync;


insert into "FieldPermission" (role, model, field, r, w) values
  (1, 'DiagHistory', 'ctime',    true, false)
, (1, 'DiagHistory', 'userId',   true, true)
, (1, 'DiagHistory', 'caseId',   true, true)
, (1, 'DiagHistory', 'slideId',  true, true)
, (1, 'DiagHistory', 'answerIx', true, true)
;


