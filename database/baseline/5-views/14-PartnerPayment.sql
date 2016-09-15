
create or replace view "PartnerPayment" as
  with delays as
    (select serviceId, partnerId, firstDelay, num as numOfDelays
      from (
        select *,
          first_value(delayMinutes) over w as firstDelay,
          row_number() over w as num,
          lead(id) over w as next_id
        from "PartnerDelay"
        window w as (partition by serviceId, partnerId order by ctime asc)
      ) x
      where next_id is null),

  services(serviceId, partnerId, delay, isCountryRide, partnerWarnedInTime) as
    (select *
      from (select
            id,
            contractor_partnerId,
            times_factServiceStart - times_expectedServiceStart as delay,
            isCountryRide,
            partnerWarnedInTime
          from techtbl) tech
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart - times_expectedServiceStart as delay,
            isCountryRide,
            partnerWarnedInTime
          from towagetbl)
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart - times_expectedServiceStart as delay,
            isCountryRide,
            partnerWarnedInTime
          from renttbl)
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart - times_expectedServiceStart as delay,
            isCountryRide,
            partnerWarnedInTime
          from taxitbl)
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart - times_expectedServiceStart as delay,
            isCountryRide,
            partnerWarnedInTime
          from sobertbl)
        union all (select
            id,
            contractor_partnerId,
            times_factServiceStart - times_expectedServiceStart as delay,
            isCountryRide,
            partnerWarnedInTime
          from averagecommissionertbl)
    ),

  services_with_delays as
    (select s.*,
        coalesce(firstDelay, 0) as firstDelay,
        coalesce(numOfDelays, 0) as numOfDelays
      from services s
        left outer join delays d
          on (s.serviceId = d.serviceId and s.partnerId = d.partnerId)
      where s.partnerId is not null
    ),

  payments as
    (select
      serviceId, partnerId,
      case
        when not isCountryRide
          and numOfDelays = 0
          and delay <= interval '0 minutes'
        then '{"val": "100% + бонус"'
          || ',"desc": "Эвакуатор приехал в назначенное время без опозданий."'
          || '}'

        when not isCountryRide
          and numOfDelays = 0
          and delay <= interval '30 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор приезжает с опозданием менее 30 минут без'
          ||           ' уведомления РАМК."'
          || '}'

        when not isCountryRide
          and numOfDelays = 0
          and delay >  interval '30 minutes'
          and delay <= interval '60 minutes'
        then '{"val": "50%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 30 минут, но'
          ||           ' менее часа без уведомления РАМК."'
          || '}'

        when not isCountryRide
          and numOfDelays = 0
          and delay >  interval '60 minutes'
        then '{"val": "0%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 1 часа без'
          ||           ' уведомления РАМК."'
          || '}'

        when not isCountryRide
          and numOfDelays = 1
          and firstDelay <= 30
          and delay <= interval '30 minutes'
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор приезжает с опозданием менее 30 минут,'
          ||           ' предварительно уведомив РАМК об опоздании до момента'
          ||           ' предположительного времени доезда."'
          || '}'

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

        when not isCountryRide
          and numOfDelays = 1
          and firstDelay > 30
          and delay <= interval '30 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 30 минут,'
          ||           ' предварительно согласовав данное опоздание с'
          ||           ' оператором РАМК и получив подтверждение."'
          || '}'

        when not isCountryRide
          and numOfDelays = 2
          and firstDelay > 30
          and delay > interval '0 minutes'
        then '{"val": "50%"'
          || ',"desc": "Эвакуатор опаздывает более, чем на 30 минут,'
          ||           ' согласовывает это опоздание с РАМК, повторно не'
          ||           ' выдерживает сроки, но уведомляет об этом РАМК."'
          || '}'

        when not isCountryRide
          and numOfDelays = 1
          and firstDelay > 30
          and delay > interval '30 minutes'
        then '{"val": "0%"'
          || ',"desc": "Эвакуатор опаздывает более, чем на 30 минут,'
          ||           ' согласовывает это опоздание с РАМК, повторно не'
          ||           ' выдерживает сроки, да ещё и не уведомляет об этом РАМК."'
          || '}'

        when not isCountryRide
          and numOfDelays = 2
          and firstDelay <= 30
          and delay <= interval '30 minutes'
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор опаздывает менее, чем на 30 минут,'
          ||           ' согласовывает это с РАМК. Затем повторно'
          ||           ' согласовывает опоздание, но при этом опоздания нет."'
          || '}'

        when isCountryRide
          and numOfDelays = 0
          and delay <= interval '0 minutes'
        then '{"val": "110%"'
          || ',"desc": "Эвакуатор за городом приехал вовремя."'
          || '}'

        when isCountryRide
          and numOfDelays >= 1
          and partnerWarnedInTime
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор за городом опаздывает и предупреждает РАМК'
          ||           ' об опоздании. РАМК согласовывает опоздание при условии'
          ||           ' доезда до клиента из расчета скорости эвакуатора."'
          || '}'

        when isCountryRide
          and numOfDelays >= 1
          and not partnerWarnedInTime
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор за городом опаздывает и предупреждает РАМК'
          ||           ' об опоздании. РАМК согласовывает опоздание при условии'
          ||           ' доезда до клиента из расчета скорости эвакуатора."'
          || '}'

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