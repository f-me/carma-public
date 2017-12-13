BEGIN;

UPDATE "SmsTemplate"

  SET isActive = TRUE
    , label = 'Оповещение партнёра'

    , text =
        'Тестовый шаблон. ' ||
        'Марка авто: $case.car_make$. ' ||
        'Модель авто: $case.car_model$. ' ||
        'Адрес места поломки: $case.breakage_address$. ' ||
        'Адрес места доставки: $service.towage.towage_to_address$. ' ||
        'Телефон клиента: $case.customer_phone$. ' ||
        'ОВНОУ: $service.times_expectedServiceStart$.'

  -- id#15 is old empty disabled template, not used anywhere now.
  -- Also added "notifyPartner" ident to "SmsTemplate" model.
  WHERE id = 15;

COMMIT;
