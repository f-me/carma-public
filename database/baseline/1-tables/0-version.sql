
create table version
  (A integer not null
  ,B integer not null
  ,C integer not null
  ,updated_at timestamp with time zone not null default now()
  ,primary key (A,B,C)
  );
