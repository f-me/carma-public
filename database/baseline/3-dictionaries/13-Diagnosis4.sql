
CREATE TABLE "Diagnosis4" (
    id SERIAL PRIMARY KEY,
    value text UNIQUE NOT NULL,
    label text UNIQUE NOT NULL
);


COPY "Diagnosis4" (id, value, label) FROM stdin;
1	diag4.1	Консультация механика
2	diag4.2	Консультация оператора
3	towage	Эвакуация
4	towCar	Техпомощь
\.

GRANT SELECT ON TABLE "Diagnosis4" TO carma_search;

