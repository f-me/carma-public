COPY "CarModel" (id, value, label, parent) FROM stdin;
399	Kyron	Kyron	48
402	Actyon	Actyon	48
403	301	301	33
404	Rexton	Rexton	48
406	Actyon Sports	Actyon Sports	48
407	Stavic	Stavic	48
408	2008	2008	33
410	Musso	Musso	48
412	Rodius	Rodius	48
413	c4L	C4L	13
414	Korando	Korando	48
415	XF	XF	50
416	CF	CF	50
418	LF	LF	50
420	Beetle	Beetle	1
423	Sandero	Sandero	34
425	Latitude	Latitude	34
427	Scenic	Scenic	34
428	Fluence	Fluence	34
429	Kangoo	Kangoo	34
430	Grand Vitara	Grand Vitara	43
431	Splash	Splash	43
432	Tucson	Tucson	19
433	IX35	IX35	19
434	Porter	Porter	19
435	Matrix	Matrix	19
436	H-1 Starex	H-1 Starex	19
437	V70	V70	45
438	XC70	XC70	45
439	XC90	XC90	45
443	CR-V	CR-V	18
444	Rezzo	Rezzo	2
445	ASX	ASX	31
447	Outlander	Outlander	31
448	Outlander XL	Outlander XL	31
449	A5	A5	10
450	Doblo	Doblo	16
452	500	500	16
453	Grand Voyager	Grand Voyager	12
454	Yeti	Yeti	40
455	Roomster	Roomster	40
456	Cordoba	Cordoba	39
458	CX7	CX7	28
459	MX5	MX5	28
460	One	One	30
461	Forester	Forester	42
462	Land Cruiser 200	Land Cruiser 200	44
463	A13	A13	51
464	SUV T11	SUV T11	51
465	S18D	S18D	51
466	Terrain	Terrain	53
467	LDV	LDV	54
468	MK	MK	55
469	911	911	59
470	Cayenne	Cayenne	59
471	214813	214813	60
472	113300	113300	60
473	California	California	61
475	Duster	Duster	34
476	Oldtimer	Oldtimer	6
477	Eco Sport	Eco Sport	6
479	Carens	Carens	22
481	i40	i40	19
482	Q7	Q7	10
486	Q3	Q3	10
487	S5	S5	10
488	A1	A1	10
489	Allroad	Allroad	10
490	Cruiser	Cruiser	2
491	RAM 1500	RAM 1500	15
492	Journey	Journey	15
493	Linea	Linea	16
494	Ducato	Ducato	16
495	Pilot	Pilot	18
496	IX55	IX55	19
497	Daimler	Daimler	20
498	21041	21041	7
499	LX	LX	25
500	RX	RX	25
501	GX	GX	25
502	CX-5	CX-5	28
503	CX-9	CX-9	28
504	RX-8	RX-8	28
505	Tribute	Tribute	28
506	ML	ML	29
507	R	R	29
508	GL	GL	29
509	Viano	Viano	29
510	Sprinter	Sprinter	29
511	GLK	GLK	29
512	CLC	CLC	29
513	CLA	CLA	29
514	Vito	Vito	29
515	G	G	29
516	Pajero	Pajero	31
517	L200	L200	31
520	Pajero Sport	Pajero Sport	31
521	Carizma	Carizma	31
522	Navara	Navara	32
525	Patrol	Patrol	32
526	Bora	Bora	1
527	SR	SR	34
529	Koleos	Koleos	34
531	Outback	Outback	42
534	Jimny	Jimny	43
535	Highlander	Highlander	44
536	Hilux	Hilux	44
537	Alphard	Alphard	44
538	Sequoia	Sequoia	44
539	Venza	Venza	44
540	XC60	XC60	45
541	V60	V60	45
543	Wall	Wall	47
544	CC6460	CC6460	47
545	CC6461	CC6461	47
546	Compass	Compass	21
547	Cherokee	Cherokee	21
548	Panamera	Panamera	59
549	911 Carrera	911 Carrera	59
550	S21	S21	51
551	Cooper	Cooper	30
552	Continental	Continental	11
553	2131	2131	7
\.

SELECT setval(pg_get_serial_sequence('"CarModel"', 'id'), max(id)) from "CarModel";
