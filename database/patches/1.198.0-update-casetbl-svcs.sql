ALTER TABLE servicetbl
ADD modelName text NOT NULL DEFAULT '';

-- Migrate casetbl services references
UPDATE servicetbl SET modelName = 'Tech' where type = 1;
UPDATE servicetbl SET modelName = 'Towage' where type =  2;
UPDATE servicetbl SET modelName = 'Rent' where type =  3;
UPDATE servicetbl SET modelName = 'Hotel' where type = 4;
UPDATE servicetbl SET modelName = 'Taxi' where type = 5;
UPDATE servicetbl SET modelName = 'SoberDriver' where type = 6;
UPDATE servicetbl SET modelName = 'Transportation' where type = 7;
UPDATE servicetbl SET modelName = 'DeliverCar' where type = 8;
UPDATE servicetbl SET modelName = 'DeliverParts' where type = 9;
UPDATE servicetbl SET modelName = 'LegalAssistance' where type =10;
UPDATE servicetbl SET modelName = 'TechInspect' where type =11;
UPDATE servicetbl SET modelName = 'Information' where type =12;
UPDATE servicetbl SET modelName = 'Consultation' where type =13;
UPDATE servicetbl SET modelName = 'Tickets' where type =14;
UPDATE servicetbl SET modelName = 'Continue' where type =15;
UPDATE servicetbl SET modelName = 'Bank' where type =16;
UPDATE servicetbl SET modelName = 'DeliverClient' where type =17;
UPDATE servicetbl SET modelName = 'AverageCommissioner' where type =18;
UPDATE servicetbl SET modelName = modelName || ':' || id::text;
CREATE TEMPORARY TABLE casetbl_services AS
SELECT parentId as caseId,array_to_string(array_agg(modelName), ',') as services
FROM servicetbl GROUP BY parentid;

UPDATE casetbl SET services = null;

UPDATE casetbl c SET services = q.services
FROM casetbl_services q WHERE q.caseId = id;
ALTER TABLE servicetbl DROP COLUMN modelName;
