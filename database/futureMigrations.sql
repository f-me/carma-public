-- set default sender instead of RAMC
alter table "Sms" alter column sender set default 'Scramble CRM';

-- FieldPermission
--  Права на звонок (снять r на все поля не из списка)

UPDATE "FieldPermission" SET r = 'f' WHERE model='Call' AND field NOT IN
('callDate', 'endDate', 'program', 'callerName_name', 'callerName_phone1', 'customerComment');


-- ServiceType (удалить всё кроме двух)

TRUNCATE "Program" CASCADE;
TRUNCATE partnertbl CASCADE;
TRUNCATE partner_servicetbl CASCADE;
DELETE FROM "ServiceType" WHERE id NOT IN (1, 2);


-- Поля кейса
UPDATE "FieldPermission" SET r = 'f' WHERE model='Case' AND field NOT IN
('id', 'callDate', 'callTaker', 'customerComment', 'contact_name', 'contact_phone1', 'program', 'subprogram', 'contractIdentifier', 'contract', 'caseAddress_coords', 'caseAddress_map', 'caseAddress_address', 'services', 'comments', 'files');

-- В postgresql.conf надо ставить datestyle = 'iso, dmy'

UPDATE "FieldPermission" SET r = 'f' WHERE model IN ('Service', 'Tech', 'Towage') AND field NOT IN
('id', 'type', 'parentId', 'createTime', 'times_expectedServiceStart', 'times_expectedServiceEnd', 'contractor_partner', 'contractor_partnerId', 'contractor_address', 'contractor_coords', 'status', 'payment_costTranscript', 'techType', 'towDealer_partner', 'towDealer_partnerId', 'towDealer_address', 'towDealer_coords');

-- Hide unused dictionaries
delete from "Dictionary" where id not in (7,21,4,11,12,45,37,34,22,9);

-- Hide bad fields
update "FieldPermission" set r = 'f' where model='ServiceType' and field in ('fdds', 'model');
