{
    "title": "Кейс",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "callDate",
            "label": "Дата звонка",
            "canWrite": true,
            "canRead": true,
            "index": true
        },
        {
            "name": "callTime",
            "label": "Время звонка",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "callTaker",
            "label": "Сотрудник РАМК",
            "canWrite": false,
            "canRead": true,
            "required": true
        },
        {
            "name": "program",
            "label": "Программа",
            "canWrite": true,
            "canRead": true,
            "required": true,
            "index": true
        },
        {
            "name": "service",
            "label": "Услуга",
            "canWrite": true,
            "canRead": true,
            "required": true,
            "index": true
        },
        {
            "name": "callSurname",
            "label": "Фамилия звонящего",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "callName",
            "label": "Имя отчество звонящего",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "ownerSurname",
            "label": "Фамилия владельца",
            "canWrite": true,
            "canRead": true,
            "index": true
        },
        {
            "name": "ownerName",
            "label": "Имя отчество владельца",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "phone",
            "label": "Мобильный телефон",
            "canWrite": true,
            "canRead": true,
            "index": true
        },
        {
            "name": "extraPhone",
            "label": "Дополнительный телефон",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "manufacturer",
            "label": "Марка автомобиля",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "model",
            "label": "Модель автомобиля",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "othermodel",
            "label": "Другая марка / модель авто",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "plateNum",
            "label": "Регистрационный номер автомобиля",
            "canWrite": true,
            "canRead": true,
            "index": true
        },
        {
            "name": "color",
            "label": "Цвет",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "vin",
            "label": "VIN автомобиля",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "vinCheck",
            "label": "VIN Проверен",
            "canWrite": true,
            "canRead": true,
            "type": "checkbox"
        },
        {
            "name": "purchased",
            "label": "Дата покупки автомобиля",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "fordInspect",
            "label": "Дата  прохождения ТО FORD",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "mileage",
            "label": "Пробег автомобиля (км)",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "inspectMileage",
            "label": "Пробег автомобиля на момент прохождения ТО (только для FORD)",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "dealer",
            "label": "Дилер продавший авто / прохождение ТО FORD",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "failure",
            "label": "Описание неисправности со слов клиента",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "failedSys",
            "label": "Система в которой произошла неисправность",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "failedDet",
            "label": "Неисправная деталь",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "breakAddr",
            "label": "Адрес места поломки",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "datePm",
            "label": "Введите дату в формате + или - число",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "whereEvac",
            "label": "Город дилера куда эвакуируют автомобиль",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "evacDealer",
            "label": "Название дилера куда эвакуируют автомобиль",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "evacAddr",
            "label": "Адрес куда эвакуируют автомобиль",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "arctimeNum",
            "label": "Номер происшествия в Arc Time",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "partnerName",
            "label": "Название партнёра",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "arrivalTime",
            "label": "Реальное время прибытия на место поломки",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "finishTime",
            "label": "Реальное время окончания услуги",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "partnerCost",
            "label": "Стоимость услуги у партнёра",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "costDetails",
            "label": "Расшифровка стоимости",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "rerun",
            "label": "Перепробег (информация от партнера)",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "sagaNum",
            "label": "Номер заказ-наряда Saga (только для VW)",
            "canWrite": true,
            "canRead": true,
            "invisible": true
        },
        {
            "name": "caseStatus",
            "label": "Статус случая от дилера (Только для VW)",
            "canWrite": true,
            "canRead": true,
            "invisible": true,
            "index": true
        },
        {
            "name": "fixDate",
            "label": "Дата окончания ремонта автомобиля у дилера (только VW)",
            "canWrite": true,
            "canRead": true,
            "invisible": true
        },
        {
            "name": "failReason",
            "label": "Описание причины неисправности со слов дилера (только VW)",
            "canWrite": true,
            "canRead": true,
            "invisible": true
        },
        {
            "name": "comment",
            "label": "Комментарий",
            "type": "textarea",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "status",
            "label": "Статус звонка",
            "canWrite": true,
            "canRead": true,
            "required": true
        }
    ]
}
