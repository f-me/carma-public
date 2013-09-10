
CREATE TABLE "NewCaseField" (
    id       SERIAL PRIMARY KEY,
    field    text NOT NULL,
    program  int NOT NULL,
    label    text NOT NULL DEFAULT '',
    info     text NOT NULL DEFAULT '',
    required bool NOT NULL DEFAULT false,
    r        bool NOT NULL DEFAULT false,
    w        bool NOT NULL DEFAULT false
);

insert into "NewCaseField" (program, field, label, r, w, required) values
  (1,'parentId', 'Ссылка на кейс', true, true, false)
, (1,'times_expectedServiceStart', 'Ожидаемое время начала оказания услуги', true, true, false)
, (1,'times_expectedServiceEnd', 'Ожидаемое время окончания оказания услуги', true, true, false)
, (1,'times_expectedDispatch', 'Время выезда партнёра', true, true, false)
, (1,'payType', 'Тип оплаты', true, true, false)
, (1,'towDealer_partner', 'Дилер (куда эвакуируют автомобиль)', true, true, false)
, (1,'towDealer_partnerId', '', true, true, false)
, (1,'towDealer_address', '', true, true, false)
, (1,'towDealer_coords', '', true, true, false)
, (1,'towerType', 'Тип эвакуатора', true, true, false)
, (1,'towType', 'Вид эвакуации', true, true, false)
, (1,'accident', 'ДТП', true, true, false)
, (1,'vandalism', 'Случай вандализма', true, true, false)
, (1,'canNeutral', 'Переключается на нейтральную передачу', true, true, false)
, (1,'towingPointPresent', 'Есть буксировочный крюк', true, true, false)
, (1,'manipulatorPossible', 'Есть место для манипулятора', true, true, false)
, (1,'companion', 'Клиент/Доверенное лицо будет сопровождать автомобиль', true, true, false)
, (1,'techType', 'Услуга', true, true, true)
, (1,'taxiFrom_address', 'Где забрать', true, true, false)
, (1,'urgentService', 'Приоритетная услуга', true, true, false)
, (1,'status', 'Статус услуги', true, true, false)
, (1,'comment', 'Что случилось', true, true, true)
, (1,'diagnosis1', 'Система', true, true, true)
, (1,'diagnosis2', 'Узел/деталь', true, true, false)
, (1,'diagnosis3', 'Описание причины неисправности', true, true, false)
, (1,'contact_name', 'Звонящий', true, true, false)
, (1,'contact_phone1', 'Контактный телефон', true, true, false)
, (1,'contact_contactOwner', 'Звонящий владелец?', true, true, false)
, (1,'contact_ownerName', 'Владелец', true, true, false)
, (1,'contact_ownerPhone1', 'Контактный телефон владельца', true, true, false)
, (1,'program', 'Программа', true, true, true)
, (1,'car_vin', 'VIN', true, true, true)
, (1,'car_make', 'Марка', true, true, true)
, (1,'car_model', 'Модель', true, true, true)
, (1,'car_seller', 'Дилер, продавший автомобиль', true, true, true)
, (1,'car_buyDate', 'Дата покупки', true, true, true)
, (1,'car_dealerTO', 'Дилер у которого проходило последнее ТО', true, true, true)
, (1,'car_mileage', 'Текущий пробег', true, true, true)
, (1,'car_checkupMileage', 'Пробег на последнем ТО', true, true, true)
, (1,'vinChecked', 'Участие в программе', true, true, true)
, (1,'car_plateNum', 'Госномер', true, true, true)
, (1,'cardNumber_cardNumber', 'Номер карты участника', true, true, false)
, (1,'car_makeYear', 'Год производства автомобиля', true, true, false)
, (1,'car_color', 'Цвет', true, true, true)
, (1,'car_transmission', 'Коробка передач', true, true, true)
, (1,'city', 'Город', true, true, true)
, (1,'caseAddress_address', 'Адрес места поломки', true, true, false)
, (1,'caseAddress_comment', 'Примечания', true, true, false)
, (1,'caseAddress_coords', 'Координаты', true, true, false)
, (1,'caseAddress_map', '', true, true, false)
, (1,'caseStatus', 'Статус кейса', true, true, true)
, (1,'services', 'Услуги', true, true, false)
, (1,'comments', 'Комментарии', true, true, false)
;
