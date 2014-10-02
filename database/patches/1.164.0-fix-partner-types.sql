drop view servicesview cascade;
drop view "Услуги" cascade;

ALTER TABLE casetbl ADD COLUMN car_seller_tmp int4;
UPDATE casetbl SET car_seller_tmp = nullif(substring(regexp_replace(car_seller, '\D', '', 'g') from 1 for 4),'')::int4;
-- Clear broken partners which resulted in numbers to avoid false
-- references
UPDATE casetbl SET car_seller_tmp = NULL WHERE coalesce(car_seller, '') <> coalesce(car_seller_tmp::text, '');
UPDATE casetbl SET car_seller_tmp = NULL WHERE NOT EXISTS (SELECT 1 FROM partnertbl WHERE id = car_seller_tmp);
UPDATE casetbl SET car_seller_tmp = p.id
FROM partnertbl p WHERE p.name = car_seller;
ALTER TABLE casetbl ALTER COLUMN car_seller TYPE int4 USING (car_seller_tmp);
ALTER TABLE casetbl DROP COLUMN car_seller_tmp;
ALTER TABLE casetbl
ADD CONSTRAINT "casetbl_car_seller_fkey"
FOREIGN KEY (car_seller) REFERENCES "partnertbl" (id);


ALTER TABLE casetbl ADD COLUMN car_dealerTO_tmp int4;
UPDATE casetbl SET car_dealerTO_tmp = nullif(substring(regexp_replace(car_dealerTO, '\D', '', 'g') from 1 for 4),'')::int4;
UPDATE casetbl SET car_dealerTO_tmp = NULL WHERE coalesce(car_dealerTO, '') <> coalesce(car_dealerTO_tmp::text, '');
UPDATE casetbl SET car_dealerTO_tmp = NULL WHERE NOT EXISTS (SELECT 1 FROM partnertbl WHERE id = car_dealerTO_tmp);
UPDATE casetbl SET car_dealerTO_tmp = p.id
FROM partnertbl p WHERE p.name = car_dealerTO;
ALTER TABLE casetbl ALTER COLUMN car_dealerTO TYPE int4 USING (car_dealerTO_tmp);
ALTER TABLE casetbl DROP COLUMN car_dealerTO_tmp;
ALTER TABLE casetbl
ADD CONSTRAINT "casetbl_car_dealerTO_fkey"
FOREIGN KEY (car_dealerTO) REFERENCES "partnertbl" (id);
