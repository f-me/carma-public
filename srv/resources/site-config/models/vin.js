{
    "name": "vin",
    "title": "VIN",
    "canCreate": false,
    "canRead": true,
    "canUpdate": false,
    "canDelete": false,
    "applications": [
        {
            "targets": true,
            "canWrite": false,
            "canRead": true
        },
        {
            "targets": [
                "car_make",
                "car_model",
                "car_makeYear",
                "car_plateNum",
                "car_color",
                "car_buyDate",
                "car_checkupDate",
                "car_seller"
            ],
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ]
        },
        {
            "targets": [
                "cardNumber_cardNumber",
                "cardNumber_serviceInterval",
                "cardNumber_validFrom",
                "cardNumber_validUntil",
                "cardNumber_mileageTO",
                "cardNumber_validUntilMilage"
            ],
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ]
        },
        {
            "targets": [
                "owner_name"
            ],
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "label": "Владелец"
            }
        },
        {
            "targets": [
                "owner_email",
                "owner_phone1"
            ],
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head","parguy" ]
        }
    ],
    "fields": [
        {
            "name": "program",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "index": true,
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Programs",
                "label": "Программа",
                "required": true,
                "targetCategory": "program",
                "infoText": "program"
            }

        },
        {
            "name": "callTaker",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "meta": {
                "label": "Сотрудник РАМК",
                "required": true,
                "readonly": true
            }
        }
    ]
}
