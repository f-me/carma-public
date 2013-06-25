create or replace function RkcCaseStats
  (_program text, _city text, _partner text, fromDate timestamp, toDate timestamp)
  returns table
    (total int
    ,mech int
    ,delay int
    ,duration int
    ,calculated int
    ,limited int
    ,satisfied int
    ) as
$$ begin
  return query with
    matchingCase as
      (select id, callDate from casetbl
        where (callDate is not null)
          and (callDate >= fromDate)
          and (_city    = '' or _city    = casetbl.city) -- NB: !!!
          and (_program = '' or _program = casetbl.program)
      ),
    matchingService as
      (select
          matchingCase.callDate,
          servicetbl.type,
          servicetbl.times_factServiceStart,
          servicetbl.times_factServiceEnd,
          servicetbl.payment_partnerCost,
          servicetbl.payment_limitedCost,
          servicetbl.clientSatisfied
        from servicetbl, matchingCase
        where ('case:' || matchingCase.id = servicetbl.parentId)
          and (servicetbl.status in ('serviceInProgress','serviceOk','serviceClosed'))
          and (_partner = '' or _partner = trim(servicetbl.contractor_partner))
          and (servicetbl.createTime is not null)
          and (servicetbl.createTime >= fromDate)
          and (servicetbl.createTime <  toDate)
      ),
    q1 as (select count(*) as total from matchingService),
    q2_1 as
      (select count(*) as mechCalls from calltbl
        where (calltbl.callerType = 'client')
          and (calltbl.callType in ('mechanicConsOk','mechanicConsNotOk'))
          and (calltbl.callDate is not null)
          and (calltbl.callDate >= fromDate)
          and (calltbl.callDate <  toDate)
          and (_program = '' or _program = calltbl.program)
          and (_city    = '' or _city    = calltbl.city)
      ),
    q2_2 as
      (select count(*) as mechServices from consultationtbl, matchingCase
      where ('case:' || matchingCase.id = consultationtbl.parentId)
        and (consultationtbl.constype = 'mech')
        and (consultationtbl.status in ('serviceInProgress','serviceOk','serviceClosed','serviceOrdered','serviceDelayed'))
        and (consultationtbl.createTime is not null)
        and (consultationtbl.createTime >= fromDate)
        and (consultationtbl.createTime <  toDate)
        and (_partner = '' or _partner = trim(consultationtbl.contractor_partner))
      ),
    q3 as
      (select extract(epoch from avg(times_factServiceStart - callDate))::int as delay
        from matchingService
        where (times_factServiceStart is not null)
          and (times_factServiceStart > callDate)
          and (type in ('towage','tech'))
      ),
    q4 as
      (select
          extract(epoch from avg(times_factServiceEnd - times_factServiceStart))::int as duration
        from matchingService
        where (times_factServiceEnd is not null)
          and (times_factServiceStart is not null)
          and (times_factServiceEnd > times_factServiceStart)
          and (type in ('towage','tech'))
      ),
    q5 as (select sum(payment_partnerCost) as calculated from matchingService),
    q6 as (select sum(payment_limitedCost) as limited from matchingService),
    q7 as (select count(*) as satisfied from matchingService where (clientSatisfied = 'satis'))
    select
        q1.total::int,
        (mechCalls + mechServices)::int,
        coalesce(q3.delay, 0)::int,
        coalesce(q4.duration, 0)::int,
        coalesce(q5.calculated, 0)::int,
        coalesce(q6.limited, 0)::int,
        case q1.total when 0 then 100
          else (q7.satisfied * 100 / q1.total)::int
          end
      from q1, q2_1, q2_2, q3, q4, q5, q6, q7;
end; $$ language plpgsql;
