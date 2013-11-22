CREATE TABLE "CarModel"
  (id     SERIAL PRIMARY KEY
  ,value  text
  ,label  text NOT NULL
  ,parent int4 REFERENCES "CarMake" ON DELETE SET NULL
  ,UNIQUE (label, parent)
  );
CREATE UNIQUE INDEX ON "CarModel" (label) WHERE parent IS NULL;

GRANT ALL ON "CarModel" TO carma_search;
GRANT ALL ON "CarModel" TO carma_db_sync;

COPY "CarModel" (id, value, label, parent) FROM stdin;
1	alfa147	147	9
2	alfa166	166	9
3	a2	A2	10
4	a3	A3	10
5	a4	A4	10
6	a6	A6	10
7	a8	A8	10
8	arnage	Arnage	11
9	continental	Continental Flying Spur	11
10	chrysler300	300	12
11	sebring	Sebring	12
12	c1	C1	13
13	c2	C2	13
14	berlingo	Berlingo	13
15	c15	C15	13
16	c3Picasso	C3 Picasso	13
17	c3Pluriel	C3 Pluriel	13
18	c4Aircross	C4 Aircross	13
19	c4Picasso	C4 Picasso	13
20	c8	C8	13
21	cCrosser	C-Crosser	13
22	cZero	C-Zero	13
23	ds3	DS3	13
24	ds4	DS4	13
25	ds5	DS5	13
26	jumper	Jumper	13
27	nemo	Nemo	13
28	saxo	Saxo	13
29	xsaraPicasso	Xsara Picasso	13
30	zx	ZX	13
31	evasionJumpy	Evasion/Jumpy	13
32	c3	C3	13
33	c4	C4	13
34	c5	C5	13
35	c6	C6	13
36	xsara	Xsara	13
37	alero	Alero	2
38	aveo	Aveo	2
39	beretta	Beretta	2
40	blazer	Blazer	2
41	camaro	Camaro	2
42	capriceCh	Caprice	2
43	captiva	Captiva	2
44	cavalier	Cavalier	2
45	celta	Celta	2
46	cheyenne	Cheyenne	2
47	cobaltCh	Cobalt	2
48	colorado	Colorado	2
49	wind	Corsa Wind	2
50	corsica	Corsica	2
51	corvette	Corvette	2
52	cruze	Cruze	2
53	epicaCh	Epica	2
54	evanda	Evanda	2
55	express	Express	2
56	orlando	Orlando	2
57	hhr	HHR	2
58	impalaSS	Impala SS	2
59	ipanemaGL	Ipanema GL	2
60	jimmy	Jimmy	2
61	lacetti	Lacetti	2
62	lanosCh	Lanos	2
63	impala	Impala	2
64	lumina	Lumina	2
65	malibu	Malibu	2
66	metro	Metro	2
67	monteCarlo	Monte Carlo	2
68	monza	Monza	2
69	niva	NIVA	2
70	prism	Prism	2
71	s10	S-10	2
72	spark	Spark	2
73	ss	SS	2
74	suburban	Suburban	2
75	tacuma	Tacuma	2
76	tahoe	Tahoe	2
77	trackerConv	Tracker Convertible	2
78	trackerHard	Tracker Hardtop	2
79	trailBlazer	Trail Blazer	2
80	Trans	Trans Sport	2
81	venture	Venture	2
82	viva	Viva	2
83	chairman	Chairman	14
84	espero	Espero	14
85	matiz	Matiz	14
86	nexia	Nexia	14
87	tosca	Tosca	14
88	caliber	Caliber	15
89	albea	Albea	16
90	bravo	Bravo	16
91	panda	Panda	16
92	punto	Punto	16
93	epica	Epica	17
94	statesman	Statesman	17
95	caprice	Caprice	17
96	accord	Accord	18
97	civic	Civic	18
98	fit	Fit	18
99	jazz	Jazz	18
100	legend	Legend	18
101	logo	Logo	18
102	i10	i10	19
103	i20	i20	19
104	i30	i30	19
105	centennial	Centennial	19
106	equus	Equus	19
107	cheryQQ	Chery QQ (Sweet)	19
108	accent	Accent	19
109	atos	Atos	19
110	elantra	Elantra	19
111	genesis	Genesis	19
112	getz	Getz	19
113	santaFe	Santa Fe new	19
114	solaris	Solaris	19
115	sonata	Sonata	19
116	xg	XG	19
117	sType	S-Type	20
118	xf	XF	20
119	xj	XJ	20
120	xType	X-Type	20
121	ceed	Cee'd	22
122	cerato	Cerato	22
123	magentis	Magentis	22
124	opirus	Opirus	22
125	picanto	Picanto	22
126	rio	Rio	22
127	spectra	Spectra	22
128	2104	2104	7
129	2107	2107	7
130	2113Samara	2113 (Samara)	7
131	2114Samara	2114 (Samara)	7
132	2115Samara	2115 (Samara)	7
133	2121Niva	2121 (Niva)	7
134	granta	Granta	7
135	kalina	Kalina	7
136	priora	Priora	7
137	largus	Largus	7
138	thesis	Thesis	23
139	defender	Defender	24
140	discavery	Discavery	24
141	freelander	Freelander	24
142	range	Range Rover	24
143	rangeEvoque	Range Rover Evoque	24
144	rangeSport	Range Rover Sport	24
145	gs	GS	25
146	is	IS	25
147	ls	LS	25
148	quattroporte	Quattroporte	26
149	mazda2	2	28
150	mazda3	3	28
151	mazda6	6	28
152	demio	Demio	28
153	aClass	A	29
154	bClass	B	29
155	cClass	C	29
156	eClass	E	29
157	sClass	S	29
158	cls	CLS	29
159	colt	Colt	31
160	galant	Galant	31
161	lancer	Lancer	31
162	proudia	Proudia	31
163	almera	Almera	32
164	cedric	Cedric	32
165	cima	Cima	32
166	laurel	Laurel	32
167	maxima	Maxima	32
168	micra	Micra	32
169	murano	Murano	32
170	note	Note	32
171	president	President	32
172	primera	Primera	32
173	pathfinder	Pathfinder	32
174	teana	Teana	32
175	xTrail	X-Trail	32
176	tiida	Tiida	32
177	ssangYong	Ssang Yong	32
178	qashqai	Qashqai	32
179	agila	Agila	3
180	antara	Antara	3
181	astra	Astra	3
182	astragtc	Astra GTC	3
183	calibra	Calibra	3
184	combo	Combo	3
185	corsa	Corsa	3
186	frontera	Frontera	3
187	insignia	Insignia	3
188	kadett	Kadett	3
189	meriva	Meriva	3
190	mokka	Mokka	3
191	monterey	Monterey	3
192	movano	Movano	3
193	omega	Omega	3
194	signum	Signum	3
195	sintra	Sintra	3
196	tigra	Tigra	3
197	vectra	Vectra	3
198	vita	Vita	3
199	vivaro	Vivaro	3
200	zafira	Zafira	3
201	allante	Allante	4
202	bls	BLS	4
203	brougham	Brougham	4
204	catera	Catera	4
205	cts	CTS	4
206	ville	DE Ville	4
207	dts	DTS	4
208	eldorado	Eldorado	4
209	escalade	Escalade	4
210	fleetwood	Fleetwood	4
211	lse	LSE	4
212	seville	Seville	4
213	srx	SRX	4
214	sts	STS	4
215	xlr	XLR	4
216	caddy	Caddy	1
217	caravelle	Caravelle	1
218	amarok	Amarok	1
219	crafter	Crafter	1
220	t5	T5	1
221	tiguan	Tiguan	1
222	polo	Polo	1
223	touareg	Touareg	1
224	passat	Passat	1
225	passatCC	Passat CC	1
226	jetta	Jetta	1
227	multivan	Multivan	1
228	golf	Golf	1
229	golfPlus	Golf Plus	1
230	sharan	Sharan	1
231	touran	Touran	1
232	phaeton	Phaeton	1
233	eos	Eos	1
234	lupo	Lupo	1
235	pointer	Pointer	1
236	transporter	Transporter	1
237	scirocco	Scirocco	1
238	ford427	427	6
239	aerostar	Aerostar	6
240	cMax	C-Max	6
241	aspire	Aspire	6
242	bronco	Bronco	6
243	sMax	S-Max	6
244	cMaxII	C-Max II	6
245	contour	Contour	6
246	cougar	Cougar	6
247	kuga	Kuga	6
248	crownVictoria	Crown Victoria	6
249	econoline	Econoline	6
250	escape	Escape	6
251	escort	Escort	6
252	escortCabrio	Escort Cabrio	6
253	escortClassic	Escort Classic	6
254	escortEstate	Escort Estate	6
255	escortHatchback	Escort Hatchback	6
256	escortTurnier	Escort Turnier	6
257	escortZX2	Escort ZX2	6
258	excursion	Excursion	6
259	expedition	Expedition	6
260	explorer	Explorer	6
261	faction	Faction	6
262	fairlane	Fairlane	6
263	falconGT	Falcon GT	6
264	fiesta	Fiesta	6
265	focus	Focus	6
266	fusion	Fusion	6
267	galaxy	Galaxy	6
268	fordGT	GT	6
269	ikon	Ikon	6
270	ka	Ka	6
271	ltd	LTD	6
272	maverick	Maverick	6
273	modelU	Model U	6
274	mondeo	Mondeo	6
275	mustang	Mustang	6
276	probe	Probe	6
277	puma	Puma	6
278	ranger	Ranger	6
279	scorpio	Scorpio	6
280	sierra	Sierra	6
281	shelbyGR	Shelby GR	6
282	sportKa	SportKa	6
283	streetKa	StreetKa	6
284	taurus	Taurus	6
285	thunderbird	Thunderbird	6
286	tourenoConnect	Tourneo Connect	6
287	transit	Transit	6
288	windstar	Windstar	6
289	1s	1 series	8
290	3s	3 series	8
291	5s	5 series	8
292	6s	6 series	8
293	7s	7 series	8
294	8s	8 series	8
295	m3	M3	8
296	m5	M5	8
297	x1	X1	8
298	x3	X3	8
299	x5	X5	8
300	x6	X6	8
301	xActivity	xActivity	8
302	z1	Z1	8
303	z3	Z3	8
304	z4	Z4	8
305	z8	Z8	8
306	107	107	33
307	206	206	33
308	106	106	33
309	208	208	33
310	309	309	33
311	508	508	33
312	807	807	33
313	1007	1007	33
314	3008	3008	33
315	4007	4007	33
316	4008	4008	33
317	5008	5008	33
318	206+	206+	33
319	bipper	Bipper	33
320	boxer	Boxer	33
321	expert	Expert	33
322	ion	Ion	33
323	partner	Partner	33
324	rcz	RCZ	33
325	207	207	33
326	307	307	33
327	308	308	33
328	407	407	33
329	408	408	33
330	607	607	33
331	clio	Clio	34
332	laguna	Laguna	34
333	logan	Logan	34
334	megane	Megane	34
335	symbol	Symbol	34
336	twingo	Twingo	34
337	velSatis	Vel Satis	34
338	phantom	Phantom	35
339	111	111	36
340	75	75	36
341	saab93	9-3	37
342	leon	Leon	39
343	fabia	Fabia	40
344	octavia	Octavia	40
345	superb	Superb	40
346	fortwo	Fortwo	41
347	impreza	Impreza	42
348	legacy	Legacy	42
349	ignis	Ignis	43
350	liana	Liana	43
351	swift	Swift	43
352	sx4	SX4	43
353	vitara	Vitara	43
354	allion	Allion	44
355	auris	Auris	44
356	avensis	Avensis	44
357	aygo	Aygo	44
358	camry	Camry	44
359	corolla	Corolla	44
360	ist	Ist	44
361	landCruiser1	Land Cruiser 1	44
362	landCruiser105	Land Cruiser 105	44
363	mark	Mark II	44
364	chaser	Chaser	44
365	cresta	Cresta	44
366	rav4	RAV-4	44
367	vitz	Vitz	44
368	yaris	Yaris	44
369	c30	C30	45
370	s40	S40	45
371	s60	S60	45
372	s80	S80	45
373	v50	V50	45
374	h1	H1	5
375	h2	H2	5
376	h3	H3	5
377	shalanda	Shalanda	46
378	5516	5516	46
379	6430A9	6430A9	46
380	4370	4370	46
381	bMax	B-Max	6
382	f250	F250	6
383	grandCMax	GRAND C-Max	6
384	f150	F150	6
385	С-Elysee	С-Elysee	13
386	wingle5	Wingle 5	47
387	C20R	C20R	47
388	C30	C30	47
389	C50	C50	47
390	Hover M2	Hover M2	47
391	Hover M4	Hover M4	47
392	Hover H6	Hover H6	47
393	Hover H5-E	Hover H5-E	47
394	cherokee	Grand Cherokee	21
395	fordEdge	Edge	6
396	usVehicles	US Vehicles	6
397	avalanche	Avalanche	2
398	juke	Juke	32
\.
