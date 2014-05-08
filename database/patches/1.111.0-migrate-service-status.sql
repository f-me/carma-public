ALTER TABLE servicetbl ADD COLUMN status_tmp int4;

UPDATE servicetbl SET status_tmp = 1 WHERE status = 'backoffice';
UPDATE servicetbl SET status_tmp = 2 WHERE status = 'creating';
UPDATE servicetbl SET status_tmp = 3 WHERE status = 'recallClient';
UPDATE servicetbl SET status_tmp = 4 WHERE status = 'clientCanceled';
UPDATE servicetbl SET status_tmp = 5 WHERE status = 'mechanicConf';
UPDATE servicetbl SET status_tmp = 6 WHERE status = 'dealerConf';
UPDATE servicetbl SET status_tmp = 7 WHERE status = 'pleaseCheck';
UPDATE servicetbl SET status_tmp = 8 WHERE status = 'checking';
UPDATE servicetbl SET status_tmp = 9 WHERE status = 'mistake';
UPDATE servicetbl SET status_tmp = 10 WHERE status = 'dealerConformation';
UPDATE servicetbl SET status_tmp = 11 WHERE status = 'makerConformation';
UPDATE servicetbl SET status_tmp = 12 WHERE status = 'orderService';
UPDATE servicetbl SET status_tmp = 13 WHERE status = 'needPartner';
UPDATE servicetbl SET status_tmp = 14 WHERE status = 'cancelService';
UPDATE servicetbl SET status_tmp = 15 WHERE status = 'serviceOrdered';
UPDATE servicetbl SET status_tmp = 16 WHERE status = 'serviceDelayed';
UPDATE servicetbl SET status_tmp = 17 WHERE status = 'serviceInProgress';
UPDATE servicetbl SET status_tmp = 18 WHERE status = 'falseCall';
UPDATE servicetbl SET status_tmp = 19 WHERE status = 'serviceOk';
UPDATE servicetbl SET status_tmp = 20 WHERE status = 'serviceClosed';

ALTER TABLE servicetbl DROP COLUMN status;
ALTER TABLE tech1tbl DROP COLUMN status;
ALTER TABLE consultationtbl DROP COLUMN status;
ALTER TABLE servicetbl ADD COLUMN status int4 REFERENCES "ServiceStatus";
UPDATE servicetbl SET status = status_tmp WHERE status_tmp IS NOT NULL;
ALTER TABLE servicetbl DROP COLUMN status_tmp;
