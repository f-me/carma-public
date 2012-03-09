{
    "name": "car",
    "title": "Информация о машине",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "vin",
            "label": "VIN",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "model",
            "label": "Марка и модель",
            "canWrite": true,
            "canRead": true,
            "type": "dictionary",
            "dictionaryName": "CarModels"
        },
        {
            "name": "plateNum",
            "label": "Регистрационный номер автомобиля",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "color",
            "label": "Цвет",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "transmission",
            "label": "Коробка передач",
            "canWrite": true,
            "canRead": true,
            "type": "dictionary",
            "dictionaryName": "Transmission"
        },
        {
            "name": "buyDate",
            "label": "Дата покупки",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "mileage",
            "label": "Текущий пробег",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "checkupDate",
            "label": "Дата последнего ТО",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "checkupMileage",
            "label": "Пробег на последнем ТО",
            "canWrite": true,
            "canRead": true
        }
    ]
}
