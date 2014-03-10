DELETE FROM "Program" WHERE label='Chevrolet Korea';
INSERT INTO "Program" (id,label) VALUES (5, 'Аларм Ассистанс');
INSERT INTO "SubProgram" (id, label, parent, value) VALUES (105, 'Основная', 5, 'alarmass');
