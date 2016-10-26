
alter table "CallerType" add column ord int not null default 0;
alter table "CallReason" add column ord int not null default 0;

select setval(pg_get_serial_sequence('"CallerType"', 'id'), max(id) + 1) from "CallerType";
select setval(pg_get_serial_sequence('"CallReason"', 'id'), max(id) + 1) from "CallReason";

update "CallerType" set ord = id;
update "CallerType" set label = 'Партнёр/Поставщик' where id = 2;
update "CallerType" set label = 'Прочие' where id = 5;

update "CallReason" set ord = id;
update "CallReason" set label = 'Проверка участия в программе/Информирование об условиях' where id = 1;
update "CallReason" set ord = 8, label = 'Иное' where id = 5;

insert into "CallReason" (parent, ord, label) values
  (1,  5, 'Помощь на дороге или заявление о ДТП')
, (1,  6, 'Отказ')
, (1,  7, 'Активация')
, (1,  9, 'Выкуп автомобиля')
, (1, 10, 'Статус зарегистрированного страхового случая или кузовного ремонта')
, (1, 11, 'Техническое обслуживание автомобиля или ремонт')
, (1, 12, 'Шиномонтаж')
, (1, 13, 'Топливные карты или телематическое оборудование')
, (1, 14, 'Доставка нового автомобиля')
, (1, 15, 'Соединить с сотрудником')
;

update "CallReason" set label = 'Соединить с бухгалтерией/Оплата счетов' where id = 7;
update "CallReason" set label = 'Иное' where id = 10;


insert into "CallReason" (parent, ord, label) values
  (2, 11, 'Согласование работ по техническому обслуживанию и слесарному ремонту')
, (2, 12, 'Шинный сервис или закупка шин')
, (2, 13, 'Кузовной ремонт')
, (2, 14, 'Продажа или доставка новых автомобилей')
, (2, 15, 'Возврат автомобиля после окончания договора аренды')
, (2, 16, 'Соединить с сотрудником')
;

insert into "CallReason" (parent, ord, label) values
  (5, 17, 'Получение услуг операционного лизинга')
, (5, 17, 'Ошибка')
;

