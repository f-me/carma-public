{
    "name": "partner",
    "title": "Партнёр",
    "canCreate": true,
    "canRead": true,
    "canUpdate": ["parguy"],
    "canDelete": ["parquy"],
     "applications": [
         {
            "targets": true,
            "canWrite": true,
            "canRead": true
         }
    ],
    "fields": [
        {
            "name": "name",
            "meta": {
                "label": "Название"
            }
        },
        {
            "name": "code",
            "meta": {
                "label": "Код"
            }
        },
        {
            "name": "city",
            "meta": {
                "label": "Город"
            }
        },
        {
            "name": "addrDeJure",
            "meta": {
                "label": "Юридический адрес"
            }
        },
        {
            "name": "addrDeFacto",
            "meta": {
                "label": "Фактический адрес"
            }
        },
        {
            "name": "openTime",
            "meta": {
                "label": "Время начала работы"
            }
        },
        {
            "name": "closeTime",
            "meta": {
                "label": "Время окончания работы"
            }
        },
        {
            "name": "phone1",
            "meta": {
                "label": "Телефоны диспетчерской"
            }
        },
        {
            "name": "fax",
            "meta": {
                "label": "Факс"
            }
        },
        {
            "name": "closeTicketPhone",
            "meta": {
                "label": "Телефон для закрытия заявок"
            }
        },
        {
            "name": "closeTicketEmail",
            "meta": {
                "label": "Email для закрытия заявок"
            }
        },
        {
            "name": "personInCharge",
            "meta": {
                "label": "Ответственное лицо"
            }
        },
        {
            "name": "taxScheme",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "TaxSchemes",
                "label": "Форма налогообложения"
            }
        },
        {
            "name": "comment",
            "meta": {
                "label": "Комментарии"
            }
        },
        {
            "name": "isPayBackConfirmed",
            "type": "checkbox",
            "meta": {
                "label": "Соглашение о вознаграждении"
            }
        },
        {
            "name": "priority1",
            "meta": {
                "label": "Приоритет за нал"
            }
        },
        {
            "name": "priority2",
            "meta": {
                "label": "Приоритет по безналу город"
            }
        },
        {
            "name": "priority3",
            "meta": {
                "label": "Приоритет по безналу за город"
            }
        },
        {
            "name": "services",
            "type": "reference"
        }
    ]
}
