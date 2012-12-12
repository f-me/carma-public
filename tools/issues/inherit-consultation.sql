alter table consultationtbl add column type text;

alter table consultationtbl add column falseCall text;
alter table consultationtbl add column warrantyCase boolean;
update consultationtbl set type = ‘consultation’;
alter table consultationtbl inherit servicetbl;
