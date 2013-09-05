CREATE TABLE "CarMake"
  (id    SERIAL PRIMARY KEY
  ,value text
  ,label text UNIQUE NOT NULL
  );

COPY "CarMake" (id, value, label) FROM stdin;
1	vw	Volkswagen
2	chevy	Chevrolet
3	opel	Opel
4	cad	Cadillac
5	hum	Hummer
6	ford	Ford
7	lada	ВАЗ (Lada)
8	bmw	BMW
9	alfa	Alfa Romeo
10	audi	Audi
11	bentley	Bentley
12	chrysler	Chrysler
13	citroen	Citroen
14	daewoo	Daewoo
15	dodge	Dodge
16	fiat	Fiat
17	holden	Holden
18	honda	Honda
19	hyundai	Hyundai
20	jaguar	Jaguar
21	jeep	Jeep
22	kia	Kia
23	lancia	Lancia
24	land	Land Rover
25	lexus	Lexus
26	maserati	Maserati
27	maybach	Maybach
28	mazda	Mazda
29	mercedes	Mercedes-Benz
30	mini	MINI
31	mitsubishi	Mitsubishi
32	nissan	Nissan
33	peugeot	Peugeot
34	renault	Renault
35	rolls	Rolls-Royce
36	rover	Rover
37	saab	Saab
38	Samand	Samand
39	seat	Seat
40	skoda	Skoda
41	smart	Smart
42	subaru	Subaru
43	suzuki	Suzuki
44	toyota	Toyota
45	volvo	Volvo
46	maz	Maz
\.
