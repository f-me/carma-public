drop table if exists "PartnerDelay_Reason";
create table "PartnerDelay_Reason"
  ( id serial primary key
  , label text not null
  );
insert into "PartnerDelay_Reason" (id, label) values
  (1, 'Другое'),
  (2, 'Дорожная ситуация (“пробка”)'),
  (3, 'ДТП'),
  (4, 'Поломка автомобиля партнера'),
  (5, 'Ж/Д переезд'),
  (6, 'Потеря связи с водителем');
grant all on "PartnerDelay_Reason" to carma_db_sync;
grant all on "PartnerDelay_Reason_id_seq" to carma_db_sync;


drop table if exists "PartnerDelay_Notified";
create table "PartnerDelay_Notified"
  ( id serial primary key
  , label text not null
  );
insert into "PartnerDelay_Notified" (id, label) values
  (1, 'Да'),
  (2, 'Нет');
grant all on "PartnerDelay_Notified" to carma_db_sync;
grant all on "PartnerDelay_Notified_id_seq" to carma_db_sync;


drop table if exists "PartnerDelay_Confirmed";
create table "PartnerDelay_Confirmed"
  ( id serial primary key
  , label text not null
  );
insert into "PartnerDelay_Confirmed" (id, label) values
  (1, 'Да'),
  (2, 'Нет'),
  (3, 'Требуется согласовать');
grant all on "PartnerDelay_Confirmed" to carma_db_sync;
grant all on "PartnerDelay_Confirmed_id_seq" to carma_db_sync;


drop table if exists "PartnerDelay_Exceptional";
create table "PartnerDelay_Exceptional"
  ( id serial primary key
  , label text not null
  );
insert into "PartnerDelay_Exceptional" (id, label) values
  (1, 'Да'),
  (2, 'Нет');
grant all on "PartnerDelay_Exceptional" to carma_db_sync;
grant all on "PartnerDelay_Exceptional_id_seq" to carma_db_sync;


drop table if exists "PartnerDelay";
create table "PartnerDelay"
  ( id serial primary key
  , ctime timestamptz not null default now()
  , caseId         int not null references casetbl
  , serviceId      int not null -- references servicetbl
  , partnerId      int not null references partnertbl
  , owner          int not null references usermetatbl
  , delayReason    int not null references "PartnerDelay_Reason"
  , delayReasonComment text
  , delayMinutes   int not null
  , notified       int not null references "PartnerDelay_Notified"
  , delayConfirmed int not null references "PartnerDelay_Confirmed"
  , exceptional    int not null references "PartnerDelay_Exceptional"
  , exceptionalComment text
  );

create index on "PartnerDelay"(caseId);

grant all on "PartnerDelay" to carma_db_sync;
grant all on "PartnerDelay_id_seq" to carma_db_sync;

