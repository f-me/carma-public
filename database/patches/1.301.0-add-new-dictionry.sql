select setval(pg_get_serial_sequence('"Dictionary"', 'id'), max(id) + 1) from "Dictionary";

insert into "Dictionary" (name, description, majorfields)
  values
    ( 'PartnerDelay_Reason'
    , 'Причина опоздания партнёра'
    , '{id, label}'
    );
