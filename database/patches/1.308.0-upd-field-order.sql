

create temporary table fields (ord sequence primary key, name text unique not null);

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
