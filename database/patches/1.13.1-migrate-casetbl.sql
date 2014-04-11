DROP VIEW servicesview;

-- Migrate car_class to CarClass dict
ALTER TABLE casetbl ADD COLUMN car_class_tmp int4;
UPDATE casetbl SET car_class_tmp = 1 WHERE car_class='a';
UPDATE casetbl SET car_class_tmp = 2 WHERE car_class='b';
UPDATE casetbl SET car_class_tmp = 3 WHERE car_class='c';
UPDATE casetbl SET car_class_tmp = 4 WHERE car_class='d';
UPDATE casetbl SET car_class_tmp = 5 WHERE car_class='e';
UPDATE casetbl SET car_class_tmp = 6 WHERE car_class='f';
ALTER TABLE casetbl DROP COLUMN car_class;
ALTER TABLE casetbl ADD COLUMN car_class int4 REFERENCES "CarClass";
UPDATE casetbl SET car_class = car_class_tmp;
ALTER TABLE casetbl DROP COLUMN car_class_tmp;

-- Migrate car_engine to Engine dict
ALTER TABLE casetbl ADD COLUMN car_engine_tmp int4;
UPDATE casetbl SET car_engine_tmp = 1 WHERE car_engine='ptrl';
UPDATE casetbl SET car_engine_tmp = 2 WHERE car_engine='dis';
ALTER TABLE casetbl DROP COLUMN car_engine;
ALTER TABLE casetbl ADD COLUMN car_engine int4 REFERENCES "Engine";
UPDATE casetbl SET car_engine = car_engine_tmp;
ALTER TABLE casetbl DROP COLUMN car_engine_tmp;

-- Migrate car_transmission to Transmission dict
ALTER TABLE casetbl ADD COLUMN car_transmission_tmp int4;
UPDATE casetbl SET car_transmission_tmp = 1 WHERE car_transmission='auto';
UPDATE casetbl SET car_transmission_tmp = 2 WHERE car_transmission='mech';
UPDATE casetbl SET car_transmission_tmp = 3 WHERE car_transmission='robot';
ALTER TABLE casetbl DROP COLUMN car_transmission;
ALTER TABLE casetbl ADD COLUMN car_transmission int4 REFERENCES "Transmission";
UPDATE casetbl SET car_transmission = car_transmission_tmp;
ALTER TABLE casetbl DROP COLUMN car_transmission_tmp;
