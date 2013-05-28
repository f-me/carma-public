
create table version
  (A integer not null
  ,B integer not null
  ,C integer not null
  ,updated_at timestamp with time zone default now()
  ,primary key (A,B,C)
  );
