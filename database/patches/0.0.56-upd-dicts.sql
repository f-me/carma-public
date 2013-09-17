update "Dictionary" set majorFields = array['id','program','label'] where id = 2;
insert into "CarMake" (id,value,label) values (47,'greatWall','Great Wall');
COPY "CarModel" (id, value, label, parent) FROM stdin;
386	wingle5	Wingle 5	47
387	C20R	C20R	47
388	C30	C30	47
389	C50	C50	47
390	Hover M2	Hover M2	47
391	Hover M4	Hover M4	47
392	Hover H6	Hover H6	47
393	Hover H5-E	Hover H5-E	47
\.
