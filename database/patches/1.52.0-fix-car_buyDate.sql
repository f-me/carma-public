ALTER TABLE casetbl ADD COLUMN car_buyDate_tmp date;
UPDATE casetbl SET car_buyDate_tmp = (car_buyDate + interval '23:59:59')::date;

ALTER TABLE casetbl DROP COLUMN car_buyDate CASCADE;
ALTER TABLE casetbl ADD COLUMN car_buyDate date;
UPDATE casetbl SET car_buyDate = car_buyDate_tmp;
ALTER TABLE casetbl DROP COLUMN car_buyDate_tmp;
