create index call_calldate_idx on calltbl (calldate);
create index call_program_idx on calltbl (program);

create index call_wazzup_trgm_idx
on calltbl
using gist(lower(wazzup) gist_trgm_ops)
where wazzup is not null;

create index call_callTaker_trgm_idx
on calltbl
using gist(lower(callTaker) gist_trgm_ops)
where callTaker is not null;

create index call_callerName_name_trgm_idx
on calltbl
using gist(lower(callerName_name) gist_trgm_ops)
where callerName_name is not null;

create index call_callerName_ownerName_trgm_idx
on calltbl
using gist(lower(callerName_ownerName) gist_trgm_ops)
where callerName_ownerName is not null;

create index call_callerName_phone1_trgm_idx
on calltbl
using gist(lower(callerName_phone1) gist_trgm_ops)
where callerName_phone1 is not null;

create index call_callerName_phone2_trgm_idx
on calltbl
using gist(lower(callerName_phone2) gist_trgm_ops)
where callerName_phone2 is not null;

create index call_callerName_phone3_trgm_idx
on calltbl
using gist(lower(callerName_phone3) gist_trgm_ops)
where callerName_phone3 is not null;

create index call_callerName_phone4_trgm_idx
on calltbl
using gist(lower(callerName_phone4) gist_trgm_ops)
where callerName_phone4 is not null;

create index call_callerName_ownerPhone1_trgm_idx
on calltbl
using gist(lower(callerName_ownerPhone1) gist_trgm_ops)
where callerName_ownerPhone1 is not null;

create index call_callerName_ownerPhone2_trgm_idx
on calltbl
using gist(lower(callerName_ownerPhone2) gist_trgm_ops)
where callerName_ownerPhone2 is not null;

create index call_callerName_ownerPhone3_trgm_idx
on calltbl
using gist(lower(callerName_ownerPhone3) gist_trgm_ops)
where callerName_ownerPhone3 is not null;

create index call_callerName_ownerPhone4_trgm_idx
on calltbl
using gist(lower(callerName_ownerPhone4) gist_trgm_ops)
where callerName_ownerPhone4 is not null;
