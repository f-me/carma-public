
-- FIXME: CTEs are not efficient if we are want to calculate partner payment
-- for single service (GET /partner/KPI/{svc}/{partner}).
-- Maybe worth rewriting this as stored procedure.

drop view "PartnerPayment";

create view "PartnerPayment" as
  with delays as
    (select serviceId, partnerId, firstDelay, num as numOfDelays
      from (
        select *,
          first_value(delayMinutes) over w as firstDelay,
          row_number() over w as num,
          lead(id) over w as next_id
        from "PartnerDelay"
        where notified = 1 -- Yes
        window w as (partition by serviceId, partnerId order by ctime asc)
      ) x
      where next_id is null),

  services(serviceId, partnerId, tmFact, tmExp, tmHist, isCountryRide, partnerWarnedInTime) as
    (select *
      from (select
            id,
            contractor_partnerId,
            times_factServiceStart,
            times_expectedServiceStart,
            times_expectedServiceStartHistory,
            isCountryRide,
            partnerWarnedInTime
          from techtbl) tech
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart,
            times_expectedServiceStart,
            times_expectedServiceStartHistory,
            isCountryRide,
            partnerWarnedInTime
          from towagetbl)
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart,
            times_expectedServiceStart,
            times_expectedServiceStartHistory,
            isCountryRide,
            partnerWarnedInTime
          from renttbl)
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart,
            times_expectedServiceStart,
            times_expectedServiceStartHistory,
            isCountryRide,
            partnerWarnedInTime
          from taxitbl)
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart,
            times_expectedServiceStart,
            times_expectedServiceStartHistory,
            isCountryRide,
            partnerWarnedInTime
          from sobertbl)
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart,
            times_expectedServiceStart,
            times_expectedServiceStartHistory,
            isCountryRide,
            partnerWarnedInTime
          from averagecommissionertbl)
    ),

  services_with_delays as
    (select s.*,
        coalesce(firstDelay, 0) as firstDelay,
        coalesce(numOfDelays, 0) as numOfDelays,
        s.tmFact
          - coalesce(
              (s.tmHist->>(json_array_length(s.tmHist) - 1)) :: timestamp at time zone 'UTC',
              s.tmExp)
          as delay,
        s.tmFact - s.tmExp as lastDelay
      from services s
        left outer join delays d
          on (s.serviceId = d.serviceId and s.partnerId = d.partnerId)
      where s.partnerId is not null
    ),

  payments as
    (select
      serviceId, partnerId,
      case
        -- 1
        when not isCountryRide
          and numOfDelays = 0
          and delay <= interval '10 minutes'
        then '{"val": "100% + бонус"'
          || ',"desc": "Эвакуатор приехал в назначенное время без опозданий."'
          || '}'

        -- 2
        when not isCountryRide
          and numOfDelays = 0
          and delay <= interval '30 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор приезжает с опозданием менее 30 минут без'
          ||           ' уведомления РАМК."'
          || '}'

        -- 3
        when not isCountryRide
          and numOfDelays = 0
          and delay >  interval '30 minutes'
          and delay <= interval '60 minutes'
        then '{"val": "50%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 30 минут, но'
          ||           ' менее часа без уведомления РАМК."'
          || '}'

        -- 4
        when not isCountryRide
          and numOfDelays = 0
          and delay >  interval '60 minutes'
        then '{"val": "0%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 1 часа без'
          ||           ' уведомления РАМК."'
          || '}'

        -- 4.x ("Показатель которого сначала не было")
        when not isCountryRide
          and numOfDelays in (1,2)
          and firstDelay <= 30
          and delay <= interval '10 minutes'
        then '{"val": "100% + бонус"'
          || ',"desc": "Нет описания."'
          || '}'

        -- 5
        when not isCountryRide
          and numOfDelays = 1
          and firstDelay <= 30
          and delay <= interval '30 minutes'
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор приезжает с опозданием менее 30 минут,'
          ||           ' предварительно уведомив РАМК об опоздании до момента'
          ||           ' предположительного времени доезда."'
          || '}'

        -- 6
        when not isCountryRide
          and numOfDelays = 2
          and firstDelay <= 30
          and delay >  interval '30 minutes'
          and delay <= interval '60 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 30 минут, но'
          ||           ' менее часа, предварительно уведомив РАМК об опоздании'
          ||           ' до момента предположительного времени доезда а также'
          ||           ' дополнительно сообщив о повторном опоздании  до'
          ||           ' согласованного срока прибытия."'
          || '}'

        -- 7
        when not isCountryRide
          and numOfDelays = 1
          and firstDelay <= 30
          and delay >  interval '30 minutes'
          and delay <= interval '60 minutes'
        then '{"val": "50%"'
          || ',"desc":"Эвакуатор приезжает с опозданием более 30 минут, но'
          ||          ' менее часа, предварительно уведомив РАМК об опоздании'
          ||          ' до момента предположительного времени доезда,'
          ||          ' дополнительно не сообщив о повторном опоздании до'
          ||          ' согласованного срока прибытия."'
          || '}'

        -- 8
        when not isCountryRide
          and numOfDelays = 2
          and firstDelay <= 30
          and delay >  interval '60 minutes'
        then '{"val": "50%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более часа,'
          ||           ' предварительно уведомив РАМК об опоздании до'
          ||           ' момента предположительного времени доезда,'
          ||           ' дополнительно сообщив о повторном опоздании до'
          ||           ' согласованного срока прибытия."'
          || '}'

        -- 9
        when not isCountryRide
          and numOfDelays = 1
          and firstDelay <= 30
          and delay >  interval '60 minutes'
        then '{"val": "0%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более часа,'
          ||           ' предварительно уведомив РАМК об опоздании до'
          ||           ' момента предположительного времени доезда,'
          ||           ' дополнительно не сообщив о повторном опоздании до'
          ||           ' согласованного срока прибытия."'
          || '}'

        -- 10
        when not isCountryRide
          and numOfDelays = 1
          and firstDelay > 30
          and lastDelay <= interval '0 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 30 минут,'
          ||           ' предварительно согласовав данное опоздание с'
          ||           ' оператором РАМК и получив подтверждение."'
          || '}'

        -- 11
        when not isCountryRide
          and numOfDelays = 2
          and firstDelay > 30
          and delay > interval '0 minutes'
        then '{"val": "50%"'
          || ',"desc": "Эвакуатор опаздывает более, чем на 30 минут,'
          ||           ' согласовывает это опоздание с РАМК, повторно не'
          ||           ' выдерживает сроки, но уведомляет об этом РАМК."'
          || '}'

        -- 12
        when not isCountryRide
          and numOfDelays = 1
          and firstDelay > 30
          and lastDelay > interval '0 minutes'
        then '{"val": "0%"'
          || ',"desc": "Эвакуатор опаздывает более, чем на 30 минут,'
          ||           ' согласовывает это опоздание с РАМК, повторно не'
          ||           ' выдерживает сроки, да ещё и не уведомляет об этом РАМК."'
          || '}'

        -- 13
        when not isCountryRide
          and numOfDelays = 2
          and firstDelay <= 30
          and delay <= interval '30 minutes'
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор опаздывает менее, чем на 30 минут,'
          ||           ' согласовывает это с РАМК. Затем повторно'
          ||           ' согласовывает опоздание, но при этом опоздания нет."'
          || '}'

        -- 14
        when isCountryRide
          and numOfDelays = 0
          and delay <= interval '0 minutes'
        then '{"val": "110%"'
          || ',"desc": "Эвакуатор за городом приехал вовремя."'
          || '}'

        -- 15.1
        when isCountryRide
          and numOfDelays >= 1
          and partnerWarnedInTime
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор за городом опаздывает и предупреждает РАМК'
          ||           ' об опоздании. РАМК согласовывает опоздание при условии'
          ||           ' доезда до клиента из расчета скорости эвакуатора."'
          || '}'

        -- 15.2
        when isCountryRide
          and numOfDelays >= 1
          and not partnerWarnedInTime
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор за городом опаздывает и предупреждает РАМК'
          ||           ' об опоздании. РАМК согласовывает опоздание при условии'
          ||           ' доезда до клиента из расчета скорости эвакуатора."'
          || '}'

        -- 16
        when isCountryRide
          and numOfDelays = 0
          and delay > interval '0 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор за городом опаздывает и не предупреждает РАМК'
          ||           ' об опоздании."'
          || '}'

        else '{}'
      end as payment
      from services_with_delays)

  select
      serviceId,
      partnerId,
      (payment::json)->>'val' as paymentPercent,
      (payment::json)->>'desc' as paymentDescription
    from payments
;

grant select on table "PartnerPayment" to reportgen;
grant select on table "PartnerPayment" to carma_db_sync;
