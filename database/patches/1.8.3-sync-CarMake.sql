COPY "CarMake" (id, value, label) FROM stdin;
48	SsangYong	SsangYong
50	daf	DAF
51	Chery	Chery
53	GMC	GMC
54	Maxus	Maxus
55	Geely	Geely
59	Porsche	Porsche
60	Lifan	Lifan
61	Ferrari	Ferrari
\.

SELECT setval(pg_get_serial_sequence('"CarMake"', 'id'), max(id)) from "CarMake";
