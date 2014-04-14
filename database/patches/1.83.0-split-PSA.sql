UPDATE "Program" SET label='Peugeot' WHERE id=1;

UPDATE "SubProgram" SET label='Гарантия' WHERE id=3;

INSERT INTO "SubProgram" (id, parent, label, value)
VALUES (117, 1, 'Service Plus (2-years)', 'psp2');
INSERT INTO "SubProgram" (id, parent, label, value)
VALUES (118, 1, 'Service Plus (3-years)', 'psp3');
INSERT INTO "SubProgram" (id, parent, label, value)
VALUES (119, 1, 'Service Optimum (3-years)', 'pso3');
INSERT INTO "SubProgram" (id, parent, label, value)
VALUES (120, 1, 'Service Maximum (3-years)', 'psm3');


INSERT INTO "Program" (id, label) VALUES (67, 'Citroen');

UPDATE "SubProgram" SET parent='67', label='Гарантия' WHERE id=4;

INSERT INTO "SubProgram" (id, parent, label, value)
VALUES (121, 67, 'Essential Drive', 'ced');
INSERT INTO "SubProgram" (id, parent, label, value)
VALUES (122, 67, 'Perfect Drive', 'cpf');
INSERT INTO "SubProgram" (id, parent, label, value)
VALUES (123, 67, 'Free Drive', 'cfd');


UPDATE casetbl SET program=67 WHERE subprogram=4;

UPDATE calltbl SET program=67 WHERE subprogram=4;
