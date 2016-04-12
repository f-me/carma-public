DROP VIEW IF EXISTS "Услуги с приоритетами";
CREATE VIEW "Услуги с приоритетами" AS
  select
      p.id            as "Номер партнера",
      p.name          as "Название партнера",
      city.label      as "Город",
      s->>'priority1' as "Приоритет за нал",
      s->>'priority2' as "Приоритет по безналу город",
      s->>'priority3' as "Приоритет по безналу за город",
      s->>'fine'      as "Процент за ложный вызов",
      svc.label       as "Услуга",
      o->>'name'      as "Тарифная опция",
      o->>'price1'    as "Стоимость за единицу за нал",
      o->>'price2'    as "Стоимость за единицу по безналу"
    from partnertbl p,
      json_array_elements(p.services) s,
      json_array_elements(s->'options') o,
      "City" city,
      "ServiceType" svc
    where p.city = city.id
      and ((s::json)->>'type')::int = svc.id
    order by p.name;

GRANT SELECT ON "Услуги с приоритетами" TO reportgen;
