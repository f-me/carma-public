

create temporary table fields (ord serial primary key, name text unique not null);
insert into fields (name) values
  ('createTime')
, ('creator')
, ('payType')
, ('warrantyCase')
, ('requestType')
, ('tasks')
, ('commAddress_address')
, ('commAddress_comment')
, ('commAddress_coords')
, ('commAddress_map')
, ('isCountryRide')
, ('suburbanMilage')
, ('totalMilage')
, ('partnerWarnedInTime')
, ('times_expectedServiceStart')
, ('times_expectedServiceStartHistory')
, ('times_expectedDispatch')
, ('times_factServiceStart')
, ('times_expectedServiceEnd')
, ('times_factServiceEnd')
, ('times_expectedServiceClosure')
, ('times_factServiceClosure')
, ('payment_costTranscript')
, ('payment_partnerCost')
, ('payment_calculatedCost')
, ('payment_limitedCost')
, ('payment_overcosted')
, ('payment_paidByRUAMC')
, ('payment_paidByClient')
, ('whatToSay1')
, ('activity')
, ('urgentService')
, ('complication')
, ('status')
;

update "ConstructorFieldOption" c
  set ord = f.ord
  from fields f
  where model = 2
    and f.name = c.field;

drop table fields;

create temporary table fields (ord serial primary key, name text unique not null);
insert into fields (name) values
  ('createTime')
, ('creator')
, ('payType')
, ('towType')
, ('towDealer_partner')
, ('towDealer_partnerId')
, ('towDealer_address')
, ('towDealer_coords')
, ('dealerDistance')
, ('towAddress_address')
, ('towAddress_comment')
, ('towAddress_coords')
, ('towAddress_map')
, ('isCountryRide')
, ('suburbanMilage')
, ('totalMilage')
, ('partnerWarnedInTime')
, ('canNeutral')
, ('towingPointPresent')
, ('manipulatorPossible')
, ('companion')
, ('check1')
, ('check2')
, ('paid')
, ('scan')
, ('original')
, ('vandalism')
, ('accident')
, ('warrantyCase')
, ('wheelsBlocked')
, ('towerType')
, ('times_expectedServiceStart')
, ('times_expectedServiceStartHistory')
, ('times_expectedDispatch')
, ('times_factServiceStart')
, ('times_expectedServiceEnd')
, ('times_factServiceEnd')
, ('times_expectedServiceClosure')
, ('times_factServiceClosure')
, ('contractor_partner')
, ('contractor_partnerLegacy')
, ('contractor_partnerId')
, ('contractor_address')
, ('contractor_coords')
, ('caseAddress_address')
, ('caseAddress_comment')
, ('caseAddress_coords')
, ('caseAddress_map')
, ('towerAddress_comment')
, ('urgentService')
, ('status')
, ('falseCall')
, ('clientCancelReason')
, ('clientSatisfied')
, ('repairEndDate')
, ('files')
;

update "ConstructorFieldOption" c
  set ord = f.ord
  from fields f
  where model = 17
    and f.name = c.field;
