BEGIN;

CREATE index actiontbl_opentime_idx ON actiontbl (opentime);
CREATE index actiontbl_assigntime_idx ON actiontbl (assigntime);

END;
