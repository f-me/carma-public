ALTER TABLE "ProcessingConfig" ADD callWaitSeconds int4 NOT NULL DEFAULT 30;

insert into "FieldPermission" (role,model,field,r,w) values (13,'ProcessingConfig', 'callWaitSeconds', 't', 't');
