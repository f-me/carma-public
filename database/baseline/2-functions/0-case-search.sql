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
        select * from casetbl where
          lower(      to_char(callDate + '4:00','DD.MM.YYYY')
            || ' ' || coalesce(comment, '')
            || ' ' || coalesce(betaComment, '')
            || ' ' || coalesce(city, '')
            || ' ' || coalesce(dealerCause, '')
            || ' ' || coalesce(contact_name, '')
            || ' ' || coalesce(contact_ownerEmail, '')
            || ' ' || coalesce(contact_ownerName, '')
            || ' ' || coalesce(car_make, '')
            || ' ' || coalesce(car_model, '')
            || ' ' || coalesce(car_seller, '')
            || ' ' || coalesce(car_dealerTO, '')
            || ' ' || coalesce(cardNumber_cardNumber, '')
            || ' ' || coalesce(cardNumber_cardOwner, '')
            || ' ' || coalesce(caseAddress_address, '')
            || ' ' || coalesce(program, '')
          ) like '%' || q || '%';
    end case;
  end;
$$ language plpgsql;

