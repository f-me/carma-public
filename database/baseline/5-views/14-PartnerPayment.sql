
-- FIXME: CTEs are not efficient if we are want to calculate partner payment
-- for single service (GET /partnerKPI/:svcid/:partnerid).
-- Maybe worth rewriting this as stored procedure.

DROP VIEW IF EXISTS "PartnerPayment";

CREATE VIEW "PartnerPayment" AS WITH

  services ( serviceId
           , partnerId
           , tmFact
           , tmExp
           , tmHist
           , isCountryRide
           , partnerWarnedInTime
           ) AS
    (SELECT * FROM (SELECT
                      id,
                      contractor_partnerId,
                      times_factServiceStart,
                      times_expectedServiceStart,
                      times_expectedServiceStartHistory,
                      isCountryRide,
                      partnerWarnedInTime
                    FROM techtbl) tech

         UNION ALL (SELECT
                      id,
                      contractor_partnerId,
                      times_factServiceStart,
                      times_expectedServiceStart,
                      times_expectedServiceStartHistory,
                      isCountryRide,
                      partnerWarnedInTime
                    FROM towagetbl)

         UNION ALL (SELECT
                      id,
                      contractor_partnerId,
                      times_factServiceStart,
                      times_expectedServiceStart,
                      times_expectedServiceStartHistory,
                      isCountryRide,
                      partnerWarnedInTime
                    FROM renttbl)

         UNION ALL (SELECT
                      id,
                      contractor_partnerId,
                      times_factServiceStart,
                      times_expectedServiceStart,
                      times_expectedServiceStartHistory,
                      isCountryRide,
                      partnerWarnedInTime
                    FROM taxitbl)

         UNION ALL (SELECT
                      id,
                      contractor_partnerId,
                      times_factServiceStart,
                      times_expectedServiceStart,
                      times_expectedServiceStartHistory,
                      isCountryRide,
                      partnerWarnedInTime
                    FROM sobertbl)

         UNION ALL (SELECT
                      id,
                      contractor_partnerId,
                      times_factServiceStart,
                      times_expectedServiceStart,
                      times_expectedServiceStartHistory,
                      isCountryRide,
                      partnerWarnedInTime
                    FROM averagecommissionertbl)
    ),

  delays_variables AS

    /*
      Определения из спецификации:

        X — Ожидаемое время начала оказания услуги (при передаче заявки)
        Y — Фактическое время начала оказания услуги
        N (numOfDelays) — Количество оповещений об опоздании

        A[1] (firstDelay) — Ожидаемое время начала оказания услуги после
                            озвучивания партнером опоздания в первый раз

        A[n] (lastDelay) — Ожидаемое время начала оказания услуги после
                           озвучивания партнером опоздания n-ый раз
                           (последнее оповещение)

      Таблица по услуге содержит историю опозданий как: `tmExp ++ [tmHist]`
      Даты сортированы по убыванию, `tmExp` - последняя дата.
    */

    (SELECT

       -- Если история опозданий пуста - то дата X - это `tmExp`
       (COALESCE( (s.tmHist->>(JSON_ARRAY_LENGTH(s.tmHist) - 1))
                    :: TIMESTAMP AT TIME ZONE 'UTC'
                , s.tmExp
                ) :: TIMESTAMP AT TIME ZONE 'UTC') AS x,

       (s.tmFact :: TIMESTAMP AT TIME ZONE 'UTC') AS y,
       JSON_ARRAY_LENGTH(s.tmHist) AS n,

       (CASE

          WHEN JSON_ARRAY_LENGTH(s.tmHist) >= 2 THEN
            (s.tmHist->>(JSON_ARRAY_LENGTH(s.tmHist) - 2))
              :: TIMESTAMP AT TIME ZONE 'UTC'

          -- Если история (`tmHist`) содержит только одну дату - значит в ней
          -- лежит самая первая дата, т.е. `X`, а `A1` - это `tmExp`.
          WHEN JSON_ARRAY_LENGTH(s.tmHist) = 1 THEN s.tmExp

        END :: TIMESTAMP AT TIME ZONE 'UTC') AS a1,

       -- `tmExp` - это последняя дата, но если история (`tmHist`) пуста -
       -- это значение будет некорректным, т.к. это будет `X`.
       (CASE
          WHEN JSON_ARRAY_LENGTH(s.tmHist) >= 1 THEN s.tmExp
        END :: TIMESTAMP AT TIME ZONE 'UTC') AS an,

       s.partnerWarnedInTime,
       s.isCountryRide,
       s.partnerId,
       s.serviceId

     FROM services s
     WHERE s.partnerId IS NOT NULL
       AND s.tmFact    IS NOT NULL -- Только оказанные услуги
    ),

  payments AS
    (SELECT
       serviceId,
       partnerId,

       CASE

         WHEN NOT isCountryRide THEN CASE

           WHEN n = 0 THEN CASE

             -- 1. n=0 ∧ y<=x+5мин
             WHEN y <= (x + interval '5 minutes')
             THEN '{ "v": "100% + бонус"'
              ||  ', "d": "Эвакуатор приехал в назначенное время '
              ||          'без опозданий"'
              ||  '}'

             -- 2. n=0 ∧ x+5мин<y<=x+30мин
             WHEN (x + interval '5 minutes') < y
              AND y <= (x + interval '30 minutes')
              AND NOT partnerWarnedInTime
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием менее 30 минут '
              ||          'без уведомления РАМК"'
              ||  '}'

             -- 3. n=0 ∧ x+30мин<y<=x+60мин
             WHEN (x + interval '30 minutes') < y
              AND y <= (x + interval '60 minutes')
              AND NOT partnerWarnedInTime
             THEN '{ "v": "50%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более 30 минут, '
              ||          'но менее часа без уведомления РАМК"'
              ||  '}'

             -- 4. n=0 ∧ y>x+60мин
             WHEN y > (x + interval '60 minutes')
              AND NOT partnerWarnedInTime
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием '
              ||          'более 1 часа без уведомления РАМК"'
              ||  '}'

           END -- of `CASE` of `n = 0` condition

           WHEN n = 1 THEN CASE

             -- 5. n=1 ∧ a[1]-x<=30мин ∧ y<=x+30мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND y <= (x + interval '30 minutes')
              AND partnerWarnedInTime
             THEN '{ "v": "100%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием менее 30 минут, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда"'
              ||  '}'

             -- 6. n=1 ∧ a[1]-x<=30мин ∧ x+30мин<y<=x+60мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND (x + interval '30 minutes') < y
              AND y <= (x + interval '60 minutes')
              AND NOT partnerWarnedInTime
             THEN '{ "v": "50%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более 30 минут, '
              ||          'но менее часа, предварительно уведомив РАМК об '
              ||          'опоздании до момента предположительного времени '
              ||          'доезда, дополнительно не сообщив о повторном '
              ||          'опоздании до согласованного срока прибытия"'
              ||  '}'

             -- 7. n=1 ∧ a[1]-x<=30мин ∧ y>x+60мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND y > (x + '60 minutes')
              AND NOT partnerWarnedInTime
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более часа, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда, '
              ||          'дополнительно не сообщив о повторном опоздании '
              ||          'до согласованного срока прибытия"'
              ||  '}'

             -- 8. n=1 ∧ a[1]-x>30мин ∧ y<=a[1]
             WHEN (a1 - x) > (interval '30 minutes')
              AND y <= a1
              AND partnerWarnedInTime
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более 30 минут, '
              ||          'предварительно согласовав данное опоздание '
              ||          'с оператором РАМК и получив подтверждение"'
              ||  '}'

             -- 9. n=1 ∧ a[1]-x>30мин ∧ y>a[1]
             WHEN (a1 - x) > (interval '30 minutes')
              AND y > a1
              AND NOT partnerWarnedInTime
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор опаздывает более, чем на 30 минут, '
              ||          'согласовывает это опоздание с РАМК, повторно '
              ||          'не выдерживает сроки, да ещё и не уведомляет '
              ||          'об этом РАМК"'
              ||  '}'

           END -- of `CASE` of `n = 1` condition

           WHEN n > 1 THEN CASE

             -- 10. n>1 ∧ a[1]-x<=30мин ∧ x+30мин<y<=x+60мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND (x + interval '30 minutes') < y
              AND y <= (x + interval '60 minutes')
              AND partnerWarnedInTime
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более 30 минут, '
              ||          'но менее часа, предварительно уведомив РАМК об '
              ||          'опоздании до момента предположительного времени '
              ||          'доезда а также дополнительно сообщив о повторном '
              ||          'опоздании до согласованного срока прибытия"'
              ||  '}'

             -- 11. n>1 ∧ a[1]-x<=30мин ∧ x+60мин<y<=a[n]
             WHEN (a1 - x) <= (interval '30 minutes')
              AND (x + interval '60 minutes') < y
              AND y <= an
              AND partnerWarnedInTime
             THEN '{ "v": "50%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более часа, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда, '
              ||          'дополнительно сообщив о повторном опоздании '
              ||          'до согласованного срока прибытия"'
              ||  '}'

             -- 12. n>1 ∧ a[1]-x<=30мин ∧ x+60мин<y>a[n]
             WHEN (a1 - x) <= (interval '30 minutes')
              AND (x + interval '60 minutes') < y
              AND y > an
              AND NOT partnerWarnedInTime
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более часа, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда, '
              ||          'дополнительно сообщив о повторном опоздании до '
              ||          'согласованного срока прибытия, но повторно '
              ||          'не выдерживает сроки."'
              ||  '}'

             -- 13. n>1 ∧ a[1]-x>30мин ∧ y<=a[n]
             WHEN (a1 - x) > (interval '30 minutes')
              AND y <= an
              AND partnerWarnedInTime
             THEN '{ "v": "50%"'
              ||  ', "d": "Эвакуатор опаздывает более чем на 30 минут, '
              ||          'согласовывает это опоздание с РАМК, повторно '
              ||          'не выдерживает сроки, но уведомляет об этом РАМК"'
              ||  '}'

             -- 14. n>1 ∧ a[1]-x>30мин ∧ y>a[n]
             WHEN (a1 - x) > (interval '30 minutes')
              AND y > an
              AND NOT partnerWarnedInTime
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор опаздывает более чем на 30 минут, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда, '
              ||          'дополнительно сообщив о повторном опоздании до '
              ||          'согласованного срока прибытия, но повторно '
              ||          'не выдерживает сроки."'
              ||  '}'

             -- 15. n>1 ∧ a[1]-x<=30мин ∧ y<=x+30мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND y <= (x + interval '30 minutes')
              AND partnerWarnedInTime
             THEN '{ "v": "100%"'
              ||  ', "d": "Эвакуатор опаздывает менее, чем на 30 минут, '
              ||          'согласовывает это с РАМК. Затем повторно '
              ||          'согласовывает опоздание, но при этом опоздания нет"'
              ||  '}'

           END -- of `CASE` of `n > 1` condition

         END -- of `CASE` of `NOT isCountryRide` condition

         WHEN isCountryRide THEN CASE

           WHEN n = 0 THEN CASE

             -- 16. n=0 ∧ y<=x+5мин
             WHEN y <= (x + interval '5 minutes')
             THEN '{ "v": "100% + бонус"'
              ||  ', "d": "Эвакуатор за городом приехал вовремя"'
              ||  '}'

             -- 17. n=0 ∧ x+5<y<=x+30мин
             WHEN (x + 5) < y
              AND y <= (x + interval '30 minutes')
              AND NOT partnerWarnedInTime
             THEN '{ "v": "100% - бонус"'
              ||  ', "d": "Эвакуатор за городом приезжает с опозданием менее '
              ||          '30 минут, не предупреждает РАМК об опоздании."'
              ||  '}'

             -- 18. n=0 ∧ y>x+30мин
             WHEN y > (x + interval '30 minutes')
              AND NOT partnerWarnedInTime
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор за городом приезжает с опозданием более '
              ||          '30 минут, не предупреждает РАМК об опоздании"'
              ||  '}'

           END -- of `CASE` of `n = 0` condition

           WHEN n > 0 THEN CASE

             -- 19. n>0 ∧ y<=a[n]
             WHEN y <= an
              AND partnerWarnedInTime
             THEN '{ "v": "100%"'
              ||  ', "d": "Эвакуатор за городом опаздывает и предупреждает '
              ||          'РАМК об опоздании. РАМК согласовывает опоздание. '
              ||          'Эвакуатор приезжает назначенное время."'
              ||  '}'

             -- 20. n>0 ∧ y>a[n]
             WHEN y > an
              AND partnerWarnedInTime
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор за городом опаздывает и предупреждает '
              ||          'РАМК об опоздании. РАМК согласовывает опоздание. '
              ||          'Эвакуатор повторно опаздывает, предупредив РАМК."'
              ||  '}'

           END -- of `CASE` of `n > 0` condition

         END -- of `CASE` of `isCountryRide` condition

       END AS payment -- of `CASE`

     FROM delays_variables
    )

  SELECT
    serviceId,
    partnerId,
    (payment::JSON)->>'v' AS paymentPercent,
    (payment::JSON)->>'d' AS paymentDescription
  FROM payments
;

GRANT SELECT ON TABLE "PartnerPayment" TO reportgen;
GRANT SELECT ON TABLE "PartnerPayment" TO carma_db_sync;
