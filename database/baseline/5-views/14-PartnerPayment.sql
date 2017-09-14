
-- FIXME: CTEs are not efficient if we are want to calculate partner payment
-- for single service (GET /partnerKPI/:svcid/:partnerid).
-- Maybe worth rewriting this as stored procedure.

DROP VIEW IF EXISTS "PartnerPayment";

CREATE VIEW "PartnerPayment" AS WITH

  delays AS (
    WITH d AS (
      SELECT
        "PartnerDelay".*,
        ROW_NUMBER() OVER (PARTITION BY serviceId ORDER BY ctime DESC) AS r
      FROM "PartnerDelay"
      ORDER BY ctime DESC
    ) SELECT d.* FROM d WHERE d.r = 1
  ),

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
                          (оповещений от партнёра, не считаются опоздания,
                           о которых партнёр не сообщил, см. флаг “предупредил”)

        A[1] (firstDelay) — Ожидаемое время начала оказания услуги после
                            озвучивания партнером опоздания в первый раз

        A[n] (lastDelay) — Ожидаемое время начала оказания услуги после
                           озвучивания партнером опоздания n-ый раз
                           (последнее оповещение)

        C — За городом (да/нет)

        W — Последние уведомление партнёра об опоздании было согласовано
            (да/нет)

        E — Исключительный случай (да/нет)

        R — Результат расчета предупреждения партнера (Вовремя/Не вовремя)

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

       (SELECT COUNT(*) AS n
        FROM "PartnerDelay" AS pd
        WHERE pd.serviceId = s.serviceId
          AND pd.notified  = 1
       ),

       (CASE

          WHEN JSON_ARRAY_LENGTH(s.tmHist) >= 2 THEN
            (s.tmHist->>(JSON_ARRAY_LENGTH(s.tmHist) - 2))
              :: TIMESTAMP AT TIME ZONE 'UTC'

          -- Если история (`tmHist`) содержит только одну дату - значит в ней
          -- лежит самая первая дата, т.е. `X`, а `A1` - это `tmExp`.
          WHEN JSON_ARRAY_LENGTH(s.tmHist) = 1 THEN s.tmExp

        END :: TIMESTAMP AT TIME ZONE 'UTC') AS a1,

       -- `tmExp` - это последняя дата, но если история (`tmHist`) пуста -
       -- это значение будет некорректным, т.к. это будет `X` (оставляем NULL).
       (CASE
          WHEN JSON_ARRAY_LENGTH(s.tmHist) >= 1 THEN s.tmExp
        END :: TIMESTAMP AT TIME ZONE 'UTC') AS an,

       (delays.delayConfirmed = 1) AS w,
       (delays.exceptional = 1) AS e,
       delays.exceptionalComment,
       s.isCountryRide AS c,

       -- По дефолту (если НЕ нажали на кнопку "расчёт") считать что вовремя
       COALESCE(s.partnerWarnedInTime, TRUE) AS r,

       s.partnerId,
       s.serviceId

     FROM services AS s
     LEFT JOIN delays ON delays.serviceId = s.serviceId
     WHERE s.partnerId IS NOT NULL
    ),

  payments AS

    (SELECT
       serviceId,
       partnerId,
       exceptionalComment,

       CASE

         -- Исключительный случай
         WHEN e THEN '{"v": "100% (исключительный случай)", "d": null}'

         WHEN NOT c THEN CASE

           WHEN n = 0 THEN CASE

             -- 1. ¬C ∧ N=0 ∧ Y<=X+5мин
             WHEN y <= (x + interval '5 minutes')
             THEN '{ "v": "100% + бонус"'
              ||  ', "d": "Эвакуатор приехал в назначенное время '
              ||          'без опозданий"'
              ||  '}'

             -- 2. ¬C ∧ N=0 ∧ X+5мин<Y<=X+30мин
             WHEN (x + interval '5 minutes') < y
              AND y <= (x + interval '30 minutes')
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием менее 30 минут '
              ||          'без уведомления РАМК"'
              ||  '}'

             -- 3. ¬C ∧ N=0 ∧ X+30мин<Y<=X+60мин
             WHEN (x + interval '30 minutes') < y
              AND y <= (x + interval '60 minutes')
             THEN '{ "v": "50%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более 30 минут, '
              ||          'но менее часа без уведомления РАМК"'
              ||  '}'

             -- 4. ¬C ∧ N=0 ∧ Y>X+60мин
             WHEN y > (x + interval '60 minutes')
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием '
              ||          'более 1 часа без уведомления РАМК"'
              ||  '}'

           END -- of `CASE` of `n = 0` condition

           WHEN n = 1 THEN CASE

             -- 5. ¬C ∧ N=1 ∧ A[1]-X<=30мин ∧ Y<=X+30мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND y <= (x + interval '30 minutes')
             THEN '{ "v": "100%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием менее 30 минут, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда"'
              ||  '}'

             -- 6. ¬C ∧ N=1 ∧ A[1]-X<=30мин ∧ X+30мин<Y<=X+60мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND (x + interval '30 minutes') < y
              AND y <= (x + interval '60 minutes')
             THEN '{ "v": "50%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более 30 минут, '
              ||          'но менее часа, предварительно уведомив РАМК об '
              ||          'опоздании до момента предположительного времени '
              ||          'доезда, дополнительно не сообщив о повторном '
              ||          'опоздании до согласованного срока прибытия"'
              ||  '}'

             -- 7. ¬C ∧ N=1 ∧ A[1]-X<=30мин ∧ Y>X+60мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND y > (x + '60 minutes')
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более часа, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда, '
              ||          'дополнительно не сообщив о повторном опоздании '
              ||          'до согласованного срока прибытия"'
              ||  '}'

             -- 8. ¬C ∧ N=1 ∧ A[1]-X>30мин ∧ Y<=A[1] ∧ W
             WHEN (a1 - x) > (interval '30 minutes')
              AND y <= a1
              AND w
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более 30 минут, '
              ||          'предварительно согласовав данное опоздание '
              ||          'с оператором РАМК и получив подтверждение"'
              ||  '}'

             -- 9. ¬C ∧ N=1 ∧ A[1]-X>30мин ∧ Y>A[1]
             WHEN (a1 - x) > (interval '30 minutes')
              AND y > a1
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор опаздывает более, чем на 30 минут, '
              ||          'согласовывает это опоздание с РАМК, повторно '
              ||          'не выдерживает сроки, да ещё и не уведомляет '
              ||          'об этом РАМК"'
              ||  '}'

           END -- of `CASE` of `n = 1` condition

           WHEN n > 1 THEN CASE

             -- 10. ¬C ∧ N>1 ∧ A[1]-X<=30мин ∧ X+30мин<Y<=X+60мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND (x + interval '30 minutes') < y
              AND y <= (x + interval '60 minutes')
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более 30 минут, '
              ||          'но менее часа, предварительно уведомив РАМК об '
              ||          'опоздании до момента предположительного времени '
              ||          'доезда а также дополнительно сообщив о повторном '
              ||          'опоздании до согласованного срока прибытия"'
              ||  '}'

             -- 11. ¬C ∧ N>1 ∧ A[1]-X<=30мин ∧ X+60мин<Y<=A[n]
             WHEN (a1 - x) <= (interval '30 minutes')
              AND (x + interval '60 minutes') < y
              AND y <= an
             THEN '{ "v": "50%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более часа, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда, '
              ||          'дополнительно сообщив о повторном опоздании '
              ||          'до согласованного срока прибытия"'
              ||  '}'

             -- 12. ¬C ∧ N>1 ∧ A[1]-X<=30мин ∧ X+60мин<Y>A[n]
             WHEN (a1 - x) <= (interval '30 minutes')
              AND (x + interval '60 minutes') < y
              AND y > an
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор приезжает с опозданием более часа, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда, '
              ||          'дополнительно сообщив о повторном опоздании до '
              ||          'согласованного срока прибытия, но повторно '
              ||          'не выдерживает сроки."'
              ||  '}'

             -- 13. ¬C ∧ N>1 ∧ A[1]-X>30мин ∧ Y<=A[n]
             WHEN (a1 - x) > (interval '30 minutes')
              AND y <= an
             THEN '{ "v": "50%"'
              ||  ', "d": "Эвакуатор опаздывает более чем на 30 минут, '
              ||          'согласовывает это опоздание с РАМК, повторно '
              ||          'не выдерживает сроки, но уведомляет об этом РАМК"'
              ||  '}'

             -- 14. ¬C ∧ N>1 ∧ A[1]-X>30мин ∧ Y>A[n]
             WHEN (a1 - x) > (interval '30 minutes')
              AND y > an
             THEN '{ "v": "0%"'
              ||  ', "d": "Эвакуатор опаздывает более чем на 30 минут, '
              ||          'предварительно уведомив РАМК об опоздании до '
              ||          'момента предположительного времени доезда, '
              ||          'дополнительно сообщив о повторном опоздании до '
              ||          'согласованного срока прибытия, но повторно '
              ||          'не выдерживает сроки."'
              ||  '}'

             -- 15. ¬C ∧ N>1 ∧ A[1]-X<=30мин ∧ Y<=X+30мин
             WHEN (a1 - x) <= (interval '30 minutes')
              AND y <= (x + interval '30 minutes')
             THEN '{ "v": "100%"'
              ||  ', "d": "Эвакуатор опаздывает менее, чем на 30 минут, '
              ||          'согласовывает это с РАМК. Затем повторно '
              ||          'согласовывает опоздание, но при этом опоздания нет"'
              ||  '}'

           END -- of `CASE` of `n > 1` condition

         END -- of `CASE` of `NOT c` condition

         WHEN c THEN CASE

           WHEN n = 0 THEN CASE

             -- 16. C ∧ N=0 ∧ Y<=X+5мин
             WHEN y <= (x + interval '5 minutes')
             THEN '{ "v": "100% + бонус"'
              ||  ', "d": "Эвакуатор за городом приехал вовремя"'
              ||  '}'

             -- 17. C ∧ N=0 ∧ X+5<Y<=X+30мин
             WHEN (x + interval '5 minutes') < y
              AND y <= (x + interval '30 minutes')
             THEN '{ "v": "100% - бонус"'
              ||  ', "d": "Эвакуатор за городом приезжает с опозданием менее '
              ||          '30 минут, не предупреждает РАМК об опоздании."'
              ||  '}'

             -- 18. C ∧ N=0 ∧ Y>X+30мин
             WHEN y > (x + interval '30 minutes')
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор за городом приезжает с опозданием более '
              ||          '30 минут, не предупреждает РАМК об опоздании"'
              ||  '}'

           END -- of `CASE` of `n = 0` condition

           WHEN n > 0 THEN CASE

             -- 19. C ∧ N>0 ∧ R
             WHEN r
             THEN '{ "v": "100%"'
              ||  ', "d": "Эвакуатор за городом опаздывает и предупреждает '
              ||          'РАМК об опоздании вовремя"'
              ||  '}'

             -- 20. C ∧ N>0 ∧ ¬R
             WHEN NOT r
             THEN '{ "v": "90%"'
              ||  ', "d": "Эвакуатор за городом опаздывает и предупреждает '
              ||          'РАМК об опоздании не вовремя"'
              ||  '}'

           END -- of `CASE` of `n > 0` condition

         END -- of `CASE` of `c` condition

       END AS payment -- of `CASE`

     FROM delays_variables
    )

  SELECT
    serviceId,
    partnerId,
    (payment::JSON)->>'v' AS paymentPercent,

    (
      CASE
        WHEN payment IS NULL THEN 'Не распознано'
        ELSE COALESCE((payment::JSON)->>'d', exceptionalComment)
      END
    ) AS paymentDescription

  FROM payments
;

GRANT SELECT ON TABLE "PartnerPayment" TO reportgen;
GRANT SELECT ON TABLE "PartnerPayment" TO carma_db_sync;
