{
    "name": "vin",
    "title": "VIN",
    "canCreate": false,
    "canRead": true,
    "canUpdate": true,
    "canDelete": false,
    "applications": [
        {
            "targets": true,
            "canWrite": true,
            "canRead": true
        }
    ],
    "fields": [
        {
            "name": "id",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "meta": {
                "label": "VIN",
                "readonly": true
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
        },
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
            "name": "owner",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "groupName": "vinContact",
            "meta": {
                "label": "Владелец",
                "infoText": "ownerName"
            }
        },
        {
            "name": "car",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "groupName": "vinCar"
        },
        {
            "name": "cardNumber",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "groupName": "cardNumber"
        }

    ]
}
