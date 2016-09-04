
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
  payments as
    (select
      s.serviceId, s.partnerId,
      case
        when not s.isCountryRide
          and d.numOfDelays = 0
          and delay <= interval '0 minutes'
        then '{"val": "100% + бонус"'
          || ',"desc": "Эвакуатор приехал в назначенное время без опозданий."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 0
          and delay <= interval '30 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор приезжает с опозданием менее 30 минут без'
          ||           ' уведомления РАМК."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 0
          and delay >  interval '30 minutes'
          and delay <= interval '60 minutes'
        then '{"val": "50%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 30 минут, но'
          ||           ' менее часа без уведомления РАМК."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 0
          and delay >  interval '60 minutes'
        then '{"val": "0%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 1 часа без'
          ||           ' уведомления РАМК."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 1
          and d.firstDelay <= 30
          and delay <= interval '30 minutes'
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор приезжает с опозданием менее 30 минут,'
          ||           ' предварительно уведомив РАМК об опоздании до момента'
          ||           ' предположительного времени доезда."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 2
          and d.firstDelay <= 30
          and delay >  interval '30 minutes'
          and delay <= interval '60 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 30 минут, но'
          ||           ' менее часа, предварительно уведомив РАМК об опоздании'
          ||           ' до момента предположительного времени доезда а также'
          ||           ' дополнительно сообщив о повторном опоздании  до'
          ||           ' согласованного срока прибытия."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 1
          and d.firstDelay <= 30
          and delay >  interval '30 minutes'
          and delay <= interval '60 minutes'
        then '{"val": "50%"'
          || ',"desc":"Эвакуатор приезжает с опозданием более 30 минут, но'
          ||          ' менее часа, предварительно уведомив РАМК об опоздании'
          ||          ' до момента предположительного времени доезда,'
          ||          ' дополнительно не сообщив о повторном опоздании до'
          ||          ' согласованного срока прибытия."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 2
          and d.firstDelay <= 30
          and delay >  interval '60 minutes'
        then '{"val": "50%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более часа,'
          ||           ' предварительно уведомив РАМК об опоздании до'
          ||           ' момента предположительного времени доезда,'
          ||           ' дополнительно сообщив о повторном опоздании до'
          ||           ' согласованного срока прибытия."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 1
          and d.firstDelay <= 30
          and delay >  interval '60 minutes'
        then '{"val": "0%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более часа,'
          ||           ' предварительно уведомив РАМК об опоздании до'
          ||           ' момента предположительного времени доезда,'
          ||           ' дополнительно не сообщив о повторном опоздании до'
          ||           ' согласованного срока прибытия."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 1
          and d.firstDelay > 30
          and delay <= interval '30 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор приезжает с опозданием более 30 минут,'
          ||           ' предварительно согласовав данное опоздание с'
          ||           ' оператором РАМК и получив подтверждение."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 2
          and d.firstDelay > 30
          and delay > interval '0 minutes'
        then '{"val": "50%"'
          || ',"desc": "Эвакуатор опаздывает более, чем на 30 минут,'
          ||           ' согласовывает это опоздание с РАМК, повторно не'
          ||           ' выдерживает сроки, но уведомляет об этом РАМК."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 1
          and d.firstDelay > 30
          and delay > interval '30 minutes'
        then '{"val": "0%"'
          || ',"desc": "Эвакуатор опаздывает более, чем на 30 минут,'
          ||           ' согласовывает это опоздание с РАМК, повторно не'
          ||           ' выдерживает сроки, да ещё и не уведомляет об этом РАМК."'
          || '}'

        when not s.isCountryRide
          and d.numOfDelays = 2
          and d.firstDelay <= 30
          and delay <= interval '30 minutes'
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор опаздывает менее, чем на 30 минут,'
          ||           ' согласовывает это с РАМК. Затем повторно'
          ||           ' согласовывает опоздание, но при этом опоздания нет."'
          || '}'

        when s.isCountryRide
          and d.numOfDelays = 0
          and delay <= interval '0 minutes'
        then '{"val": "110%"'
          || ',"desc": "Эвакуатор за городом приехал вовремя."'
          || '}'

        when s.isCountryRide
          and d.numOfDelays >= 1
          and s.partnerWarnedInTime
        then '{"val": "100%"'
          || ',"desc": "Эвакуатор за городом опаздывает и предупреждает РАМК'
          ||           ' об опоздании. РАМК согласовывает опоздание при условии'
          ||           ' доезда до клиента из расчета скорости эвакуатора."'
          || '}'

        when s.isCountryRide
          and d.numOfDelays >= 1
          and not s.partnerWarnedInTime
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор за городом опаздывает и предупреждает РАМК'
          ||           ' об опоздании. РАМК согласовывает опоздание при условии'
          ||           ' доезда до клиента из расчета скорости эвакуатора."'
          || '}'

        when s.isCountryRide
          and d.numOfDelays = 0
          and delay > interval '0 minutes'
        then '{"val": "90%"'
          || ',"desc": "Эвакуатор за городом опаздывает и не предупреждает РАМК'
          ||           ' об опоздании."'
          || '}'

        else '{"val": "−"'
          || ',"desc": "Неизвестно"'
          || '}'
      end as payment
      from services s
        left outer join delays d
          on (s.serviceId = d.serviceId and s.partnerId = d.partnerId)
      where s.partnerId is not null)

  select
      serviceId,
      partnerId,
      (payment::json)->>'val' as paymentPercent,
      (payment::json)->>'desc' as paymentDescription
    from payments
;

grant select on table "PartnerPayment" to reportgen;
