
CREATE TABLE "DefaultNewCaseField" (
    id       SERIAL PRIMARY KEY,
    field    text UNIQUE NOT NULL,
    label    text NOT NULL DEFAULT '',
    info     text NOT NULL DEFAULT '',
    required bool NOT NULL DEFAULT false,
    r        bool NOT NULL DEFAULT false,
    w        bool NOT NULL DEFAULT false
);

GRANT ALL ON "DefaultNewCaseField" TO carma_db_sync;
GRANT ALL ON "DefaultNewCaseField" TO carma_search;

insert into "DefaultNewCaseField" (field, label, r, w, required) values
  ('parentId', 'Ссылка на кейс', true, true, false)
, ('times_expectedServiceStart', 'Ожидаемое время начала оказания услуги', true, true, false)
, ('times_expectedServiceEnd', 'Ожидаемое время окончания оказания услуги', true, true, false)
, ('times_expectedDispatch', 'Время выезда партнёра', true, true, false)
, ('payType', 'Тип оплаты', true, true, false)
, ('towDealer_partner', 'Дилер (куда эвакуируют автомобиль)', true, true, false)
, ('towDealer_partnerId', '', true, true, false)
, ('towDealer_address', '', true, true, false)
, ('towDealer_coords', '', true, true, false)
, ('towerType', 'Тип эвакуатора', true, true, false)
, ('towType', 'Вид эвакуации', true, true, false)
, ('accident', 'ДТП', true, true, false)
, ('vandalism', 'Случай вандализма', true, true, false)
, ('canNeutral', 'Переключается на нейтральную передачу', true, true, false)
, ('towingPointPresent', 'Есть буксировочный крюк', true, true, false)
, ('manipulatorPossible', 'Есть место для манипулятора', true, true, false)
, ('companion', 'Клиент/Доверенное лицо будет сопровождать автомобиль', true, true, false)
, ('techType', 'Услуга', true, true, true)
, ('taxiFrom_address', 'Где забрать', true, true, false)
, ('urgentService', 'Приоритетная услуга', true, true, false)
, ('status', 'Статус услуги', true, true, false)
, ('comment', 'Что случилось', true, true, true)
, ('diagnosis1', 'Система', true, true, true)
, ('diagnosis2', 'Узел/деталь', true, true, false)
, ('diagnosis3', 'Описание причины неисправности', true, true, false)
, ('contact_name', 'Звонящий', true, true, false)
, ('contact_phone1', 'Контактный телефон', true, true, false)
, ('contact_contactOwner', 'Звонящий владелец?', true, true, false)
, ('contact_ownerName', 'Владелец', true, true, false)
, ('contact_ownerPhone1', 'Контактный телефон владельца', true, true, false)
, ('program', 'Программа', true, true, true)
, ('car_vin', 'VIN', true, true, true)
, ('car_make', 'Марка', true, true, true)
, ('car_model', 'Модель', true, true, true)
, ('car_seller', 'Дилер, продавший автомобиль', true, true, true)
, ('car_buyDate', 'Дата покупки', true, true, true)
, ('car_dealerTO', 'Дилер у которого проходило последнее ТО', true, true, true)
, ('car_mileage', 'Текущий пробег', true, true, true)
, ('car_checkupMileage', 'Пробег на последнем ТО', true, true, true)
, ('vinChecked', 'Участие в программе', true, true, true)
, ('car_plateNum', 'Госномер', true, true, true)
, ('cardNumber_cardNumber', 'Номер карты участника', true, true, false)
, ('car_makeYear', 'Год производства автомобиля', true, true, false)
, ('car_color', 'Цвет', true, true, true)
, ('car_transmission', 'Коробка передач', true, true, true)
, ('city', 'Город', true, true, true)
, ('caseAddress_address', 'Адрес места поломки', true, true, false)
, ('caseAddress_comment', 'Примечания', true, true, false)
, ('caseAddress_coords', 'Координаты', true, true, false)
, ('caseAddress_map', '', true, true, false)
, ('caseStatus', 'Статус кейса', true, true, true)
, ('services', 'Услуги', true, true, false)
, ('comments', 'Комментарии', true, true, false)
;
