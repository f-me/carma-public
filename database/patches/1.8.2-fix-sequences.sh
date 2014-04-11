#!/bin/bash -e
#CHECKOUT=904be9f955d8a8f4b498c0f3f98cef6c3ed8b3dd/1.8.1-fix-sequences.sh
$PSQL -c 'drop view if exists servicesview;'
$PSQL -c 'drop view if exists partnercancelview;'
$PSQL -c 'drop table if exists "VinFormat"'
$PSQL -c 'drop table if exists "Contract"'
$PSQL -c 'drop table if exists "LegalForm"'
$PSQL -c 'drop table if exists "CarClass"'
$PSQL -c 'drop table if exists "CheckType"'
$PSQL -c 'drop table if exists "Transmission"'
$PSQL -c 'drop table if exists "Engine"'
$PSQL -c 'drop table if exists "SubProgram"'

$PSQL -c 'drop table if exists "ServiceInfo"'
$PSQL -c 'drop table if exists "ServiceNames"'
$PSQL -c 'drop table if exists "ProgramInfo"'
$PSQL -c 'drop table if exists "Program"'
$PSQL -c 'drop table if exists "ProgramType"'

$PSQL -c 'drop table if exists "Sms"'
$PSQL -c 'drop table if exists "SmsTemplate"'
$PSQL -c 'drop table if exists "Diagnosis4"'
$PSQL -c 'drop table if exists "Diagnosis3"'
$PSQL -c 'drop table if exists "Diagnosis2"'
$PSQL -c 'drop table if exists "Diagnosis1"'
$PSQL -c 'drop table if exists "Diagnosis0"'
$PSQL -c 'drop table if exists "City"'
$PSQL -c 'drop table if exists "CarModel"'
$PSQL -c 'drop table if exists "CarMake"'

$PSQL -f baseline/3-dictionaries/4-CarMake.sql
$PSQL -f baseline/3-dictionaries/5-CarModel.sql
$PSQL -f baseline/3-dictionaries/6-City.sql
$PSQL -f baseline/3-dictionaries/9-Diagnosis0.sql
$PSQL -f baseline/3-dictionaries/10-Diagnosis1.sql
$PSQL -f baseline/3-dictionaries/11-Diagnosis2.sql
$PSQL -f baseline/3-dictionaries/12-Diagnosis3.sql
$PSQL -f baseline/3-dictionaries/13-Diagnosis4.sql
$PSQL -f baseline/3-dictionaries/18-SmsTemplate.sql
$PSQL -f baseline/3-dictionaries/27-Sms.sql

$PSQL -f baseline/3-dictionaries/21-ProgramType.sql
$PSQL -f baseline/3-dictionaries/22-Program.sql
$PSQL -f baseline/3-dictionaries/19-ProgramInfo.sql
$PSQL -f baseline/3-dictionaries/21-ServiceNames.sql
$PSQL -f baseline/3-dictionaries/20-ServiceInfo.sql

$PSQL -f baseline/3-dictionaries/23-SubProgram.sql
$PSQL -f baseline/3-dictionaries/27-Engine.sql
$PSQL -f baseline/3-dictionaries/29-Transmission.sql
$PSQL -f baseline/3-dictionaries/31-CheckType.sql
$PSQL -f baseline/3-dictionaries/32-CarClass.sql
$PSQL -f baseline/3-dictionaries/33-LegalForm.sql
$PSQL -f baseline/3-dictionaries/34-Contract.sql
$PSQL -f baseline/3-dictionaries/35-VinFormat.sql
$PSQL -f baseline/5-views/1-partnercancel-view.sql
$PSQL -f baseline/5-views/2-services-view.sql
