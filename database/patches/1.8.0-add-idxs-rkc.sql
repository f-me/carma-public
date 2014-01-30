DROP INDEX IF EXISTS actiontbl_closetime_idx;
create index actiontbl_closetime_idx on actiontbl (closetime);

DROP INDEX IF EXISTS actiontbl_duetime_full_idx;
create index actiontbl_duetime_full_idx on actiontbl (duetime);

DROP INDEX IF EXISTS casetbl_city_idx;
create index casetbl_city_idx on casetbl (city);
