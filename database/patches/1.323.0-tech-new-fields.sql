BEGIN;

-- Make space for everything under isCountryRide
UPDATE "ConstructorFieldOption" SET ord = ord * 5 WHERE model = 14 AND ord > 14;

ALTER TABLE techtbl ADD COLUMN compl27p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl27p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 1, 'compl27p1', 'Двери открываются', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl27p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl27p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 2, 'compl27p2', 'Капот открывается', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl27p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl27p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 3, 'compl27p3', 'Есть место для подъезда ТП спереди/ сбоку 6 метров', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl27p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl27p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 4, 'compl27p4', 'Сложность compl27p4', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl27p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl27p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 5, 'compl27p5', 'Сложность compl27p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl29p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl29p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 6, 'compl29p1', 'Двери открываются', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl29p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl29p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 7, 'compl29p2', 'Капот открывается', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl29p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl29p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 8, 'compl29p3', 'Есть место для подъезда ТП спереди/ сбоку 6 метров', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl29p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl29p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 9, 'compl29p4', 'Авто находится на подземном паркинге', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl29p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl29p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 10, 'compl29p5', 'Сложность compl29p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl28p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl28p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 11, 'compl28p1', 'Автомобиль не посреди дороги/ автомагистрали', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl28p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl28p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 12, 'compl28p2', 'Автомобиль стоит на твердом ровном покрытии, без уклона', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl28p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl28p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 13, 'compl28p3', 'Есть запасное колесо', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl28p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl28p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 14, 'compl28p4', 'Секреток нет', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl28p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl28p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 15, 'compl28p5', 'Есть ключ от секретки', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl32p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl32p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 16, 'compl32p1', 'Ключ в автомобиле', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl32p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl32p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 17, 'compl32p2', 'Двигатель не заведен', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl32p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl32p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 18, 'compl32p3', 'Есть дополнительное запирающее устройство капота', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl32p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl32p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 19, 'compl32p4', 'Есть документы на автомобиль', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl32p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl32p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 20, 'compl32p5', 'Сложность compl32p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl33p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl33p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 21, 'compl33p1', 'Двери открываются', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl33p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl33p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 22, 'compl33p2', 'Сложность compl33p2', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl33p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl33p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 23, 'compl33p3', 'Сложность compl33p3', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl33p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl33p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 24, 'compl33p4', 'Сложность compl33p4', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl33p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl33p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 25, 'compl33p5', 'Сложность compl33p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl31p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl31p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 26, 'compl31p1', 'Открывается лючок бензобака', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl31p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl31p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 27, 'compl31p2', 'Сложность compl31p2', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl31p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl31p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 28, 'compl31p3', 'Сложность compl31p3', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl31p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl31p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 29, 'compl31p4', 'Сложность compl31p4', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl31p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl31p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 30, 'compl31p5', 'Сложность compl31p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl35p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl35p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 31, 'compl35p1', 'Авто на твердой поверхности', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl35p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl35p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 32, 'compl35p2', 'Авто на колесах', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl35p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl35p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 33, 'compl35p3', 'Расстояние от авто до дороги меньше 10 метров', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl35p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl35p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 34, 'compl35p4', 'Сложность compl35p4', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl35p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl35p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 35, 'compl35p5', 'Сложность compl35p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl34p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl34p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 36, 'compl34p1', 'Высота потолка ниже 2м20 см', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl34p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl34p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 37, 'compl34p2', 'Сложность compl34p2', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl34p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl34p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 38, 'compl34p3', 'Сложность compl34p3', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl34p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl34p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 39, 'compl34p4', 'Сложность compl34p4', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl34p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl34p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 40, 'compl34p5', 'Сложность compl34p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl37p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl37p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 41, 'compl37p1', 'Сложность compl37p1', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl37p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl37p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 42, 'compl37p2', 'Сложность compl37p2', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl37p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl37p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 43, 'compl37p3', 'Сложность compl37p3', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl37p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl37p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 44, 'compl37p4', 'Сложность compl37p4', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl37p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl37p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 45, 'compl37p5', 'Сложность compl37p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl36p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl36p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 46, 'compl36p1', 'Топливо вытекает, когда двигатель заведен', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl36p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl36p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 47, 'compl36p2', 'Топливо вытекает когда двигатель заглушен', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl36p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl36p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 48, 'compl36p3', 'Сложность compl36p3', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl36p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl36p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 49, 'compl36p4', 'Сложность compl36p4', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl36p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl36p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 50, 'compl36p5', 'Сложность compl36p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl41p1 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl41p1', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 51, 'compl41p1', 'Наличие запасных частей', 't', 't' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl41p2 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl41p2', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 52, 'compl41p2', 'Сложность compl41p2', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl41p3 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl41p3', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 53, 'compl41p3', 'Сложность compl41p3', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl41p4 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl41p4', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 54, 'compl41p4', 'Сложность compl41p4', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

ALTER TABLE techtbl ADD COLUMN compl41p5 bool ;
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (1, 'Tech', 'compl41p5', 't', 't');
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w) (SELECT 14, program, ord + 55, 'compl41p5', 'Сложность compl41p5', 'f', 'f' FROM "ConstructorFieldOption" WHERE model=14 AND field='isCountryRide');

COMMIT;
