
-- These prevent sequential scans in "CaseHistory" view.
create index on "Sms"(caseRef);
create index on "Event"(modelId);
create index on "PartnerCancel"(caseId);
create index on "AvayaEvent"(currentAction);
