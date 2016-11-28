

drop table if exists "DiagHistory";
create table "DiagHistory"
  ( id serial primary key
  , ctime timestamptz not null default now()
  , owner int not null references usermetatbl(id)
  , caseId int not null references casetbl(id)
   --v references DiagSlide but can be invalid due to deleted slide
  , slideId int not null
  , question text not null
  , answer text not null
  , isFinal bool not null default false
  );

grant all on "DiagHistory" to carma_db_sync;
grant all on "DiagHistory_id_seq" to carma_db_sync;


drop table if exists "DiagSlide";
create table "DiagSlide"
  ( id serial primary key
  , ctime timestamptz not null default now()
  , header text not null
  , body text not null
  , resources json not null default '[]'::json
  , answers json not null default '[]'::json
  , isRoot bool not null default false
  );

insert into "DiagSlide"
  (header, body, isRoot) values
  ('Новый вопрос', '?', true);

grant all on "DiagSlide" to carma_db_sync;
grant all on "DiagSlide_id_seq" to carma_db_sync;
