update calltbl c
  set enddate = a.closeTime
  from actiontbl a
  where a.type = 100 -- ActionType.call
    and a.callid = c.id
    and c.enddate is null
    and c.calldate > '2015-06-01'; -- call actions deployed
