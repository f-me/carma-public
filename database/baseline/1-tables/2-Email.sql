create table "Email"
  ( id serial primary key
  , ctime timestamp with time zone not null default now()
  , mtime timestamp with time zone not null default now()
  , "from" text not null
  , "to" text[] not null
  , cc text[] not null default array[]::text[]
  , reply text
  , mime text not null
  , subject text not null
  , body text not null
  , status text not null
  , error text
  , why json not null
  );

GRANT ALL ON "Email" TO mail_svc;
GRANT ALL ON "Email" TO carma_db_sync;
GRANT ALL ON "Email_id_seq" TO carma_db_sync;
