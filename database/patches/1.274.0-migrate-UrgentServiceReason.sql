ALTER TABLE servicetbl ADD COLUMN urgentService_tmp int4 REFERENCES "UrgentServiceReason";

UPDATE servicetbl SET urgentService_tmp = 1 WHERE urgentService = 'notUrgent';
UPDATE servicetbl SET urgentService_tmp = 2 WHERE urgentService = 'priority1';
UPDATE servicetbl SET urgentService_tmp = 3 WHERE urgentService = 'priority2';
UPDATE servicetbl SET urgentService_tmp = 4 WHERE urgentService = 'priority3';
UPDATE servicetbl SET urgentService_tmp = 5 WHERE urgentService = 'priority4';
UPDATE servicetbl SET urgentService_tmp = 6 WHERE urgentService = 'priority5';
UPDATE servicetbl SET urgentService_tmp = 7 WHERE urgentService = 'kommissar';

ALTER TABLE servicetbl DROP COLUMN urgentService;
ALTER TABLE consultationtbl DROP COLUMN urgentService;
ALTER TABLE tech1tbl DROP COLUMN urgentService;
ALTER TABLE servicetbl ADD COLUMN urgentService int4 REFERENCES "UrgentServiceReason";
UPDATE servicetbl SET urgentService = urgentService_tmp;
ALTER TABLE servicetbl DROP COLUMN urgentService_tmp;
