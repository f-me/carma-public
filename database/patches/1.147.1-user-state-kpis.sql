CREATE OR REPLACE FUNCTION get_KPI_timeinstate(u_id integer[], range tstzrange)
 RETURNS TABLE (
 userid         integer,
 "LoggedOut"    interval,
 "Ready"        interval,
 "Rest"         interval,
 "Busy"         interval,
 "Dinner"       interval,
 "ServiceBreak" interval,
 "totalRest"    interval,
 "totalLoggedIn"   interval) AS
$func$
BEGIN
  RETURN QUERY
SELECT *,
   coalesce(ct."Rest",         interval '0') +
   coalesce(ct."Dinner",       interval '0') +
   coalesce(ct."ServiceBreak", interval '0') as totalRest,

   coalesce(ct."Rest",         interval '0') +
   coalesce(ct."Dinner",       interval '0') +
   coalesce(ct."ServiceBreak", interval '0') +
   coalesce(ct."Busy",         interval '0') +
   coalesce(ct."Ready",        interval '0') as totalLoggedIn
   FROM
crosstab( $$
   SELECT us.userid, us.state,
   sum( upper('$$||range||$$' * us.range)
      - lower('$$||range||$$' * us.range)) as "timeInState"
   FROM "UserState" us
   WHERE us.userid = any('$$ || u_id::text || $$')
   AND '$$|| range ||$$' && us.range
  GROUP BY us.userid, us.state
  ORDER BY 1,2
$$,
$$
SELECT unnest (array['LoggedOut',
                     'Ready',
                     'Rest',
                     'Busy',
                     'Dinner',
                     'ServiceBreak'])
$$) AS ct(userid int,
          "LoggedOut" interval,
          "Ready" interval,
          "Rest" interval,
          "Busy" interval,
          "Dinner" interval,
          "ServiceBreak" interval);
END;
$func$
LANGUAGE plpgsql;
