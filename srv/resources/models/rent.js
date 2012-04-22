{
    "name": "rent",
    "title": "Подменный автомобиль",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": ["rentAddress_address"],
            "meta": {
                "label": "Куда доставить"
            }
        },
        {
            "targets": ["towDealer_partner"],
            "meta": {
                "label": "Дилер"
            }
        },
        {
            "targets": ["rentedCar_rentedModel"],
            "meta": {
                "dictionaryParent": "car_make"
            }
        }        
    ],
    "fields": [
        {
            "name": "status",
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        },
        {
            "name": "towDealer",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "partner"
        },
        {
            "name": "rentAddress",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "address"
        },
        {
            "name": "carClass",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "CarClasses",
                "label": "Класс автомобиля"
            }
        },
        {
            "name": "rentContractor",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "partner",
            "meta": {
                "label": "Подрядчик"
            }
        }
        ,
        {
            "name": "carProvidedFor",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "label": "Срок, на который предоставлен автомобиль (дней)"
            }
        },
                {
            "name": "rentedMake",
            "meta": {
                "dictionaryName": "CarMakers",
                "label": "Марка, предоставленного автомобиля"
            },
            "type": "dictionary"
        },
        {
            "name": "rentedModel",
            "meta": {
                "dictionaryName": "CarModels",
                "label": "Модель, предоставленного автомобиля"
            },
            "type": "dictionary"
        }
    ]
}
