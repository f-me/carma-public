
drop view "Услуги с приоритетами";
drop view "Партнеры";
alter table partnertbl drop column services;
alter table partnertbl add column services json not null default '[]'::json;

with services as
  (select
      p.id,
      (select array_to_json(array_agg(row_to_json(j_s.*)))
        from
          (select
              servicename as type,
              priority1,
              priority2,
              priority3,
              falseCallPercent as fine,
              coalesce(
                (select array_to_json(array_agg(row_to_json(j_o.*)))
                  from
                    (select optionname as name, price1, price2
                      from tarifoptiontbl o
                      where coalesce(o.parentid, '') <> ''
                        and coalesce(o.optionname, '') <> ''
                        and s.id = replace(o.parentid, 'partner_service:', '')::int
                    ) j_o
                ),
                '[]'::json
              ) as options
            from "PartnerService" s
            where p.id = s.parentid
          ) j_s
      ) as services
    from partnertbl p
    group by p.id)
  update partnertbl p
    set services = s.services
    from services s
    where s.id = p.id and s.services is not null;

-- drop table "PartnerService";
-- drop table partner_servicetbl;
-- drop table tarifoptiontbl;
