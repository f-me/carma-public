create or replace function CaseSearch (query text) returns setof casetbl as
$$
  declare
    q   text = lower(trim(query));
    tag text = substring(q from '!(.*):.*');
    val text = substring(q from '!.*:(.*)');
    pat text = '%' || val || '%';
  begin
    case tag
      when 'кейс' then
        begin
          return query select * from casetbl where id = val::int;
        exception when invalid_text_representation then
          -- do nothing
        end;
      when 'vin' then return query
        select * from casetbl where lower(car_vin) like pat;
      when 'госномер' then return query
        select * from casetbl where lower(car_plateNum) like pat;
      when 'тел' then return query
        select * from casetbl
          where  (contact_phone1      is not null and lower(contact_phone1)      like pat)
              or (contact_phone2      is not null and lower(contact_phone2)      like pat)
              or (contact_phone3      is not null and lower(contact_phone3)      like pat)
              or (contact_phone4      is not null and lower(contact_phone4)      like pat)
              or (contact_ownerPhone1 is not null and lower(contact_ownerPhone1) like pat)
              or (contact_ownerPhone2 is not null and lower(contact_ownerPhone2) like pat)
              or (contact_ownerPhone3 is not null and lower(contact_ownerPhone3) like pat);
      else return query
        select casetbl.* from casetbl
        left join "City" on "City".id = casetbl.city
        left join "CarMake" on "CarMake".id = casetbl.car_make
        left join "CarModel" on "CarModel".id = casetbl.car_model
        left join partnertbl ps on ps.id = casetbl.car_seller
        left join partnertbl pd on pd.id = casetbl.car_dealerTO
        where
          (      to_char(callDate + '4:00','DD.MM.YYYY')
            || ' ' || coalesce(customerComment, '')
            || ' ' || coalesce("City".label, '')
            || ' ' || coalesce(dealerCause, '')
            || ' ' || coalesce(contact_name, '')
            || ' ' || coalesce(contact_ownerEmail, '')
            || ' ' || coalesce(contact_ownerName, '')
            || ' ' || coalesce("CarMake".label, '')
            || ' ' || coalesce("CarModel".label, '')
            || ' ' || coalesce(ps.name, '')
            || ' ' || coalesce(pd.name, '')
            || ' ' || coalesce(caseAddress_address, '')
          ) ilike '%' || q || '%';
    end case;
  end;
$$ language plpgsql;
