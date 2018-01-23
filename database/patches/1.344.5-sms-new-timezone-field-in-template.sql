BEGIN;

UPDATE "SmsTemplate"

  SET text =
        'Тестовый шаблон. ' ||
        'Марка авто: $case.car_make$. ' ||
        'Модель авто: $case.car_model$. ' ||
        'Адрес места поломки: $case.breakage_address$. ' ||
        'Адрес места доставки: $service.towage.towage_to_address$. ' ||
        'Телефон клиента: $case.customer_phone$. ' ||
        'ОВНОУ: $service.times_expectedServiceStart_caseCityTZ$.'

  -- See also 1.344.0-sms-template-notify-partner.sql for details
  WHERE id = 15;

COMMIT;
