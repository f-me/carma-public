-- set default sender instead of RAMC
alter table "Sms" alter column sender set default 'Scramble CRM';
