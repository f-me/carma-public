{
    "name": "case",
    "title": "Кейс",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": [
                "caseAddress_address"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "meta": {
                "label": "Адрес места поломки",
                "infoText": "caseAddress"
            }
        },
        {
            "targets": [
                "contact_name"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "meta": {
                "label": "Звонящий"
            }
        },
        {
            "targets": [
                "cardNumber_cardNumber"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Карта участника",
                "infoText": "cardnum"
            }
        },
        {
            "targets": [
                "caseAddress_coords",
                "caseAddress_city",
                "caseAddress_comment"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "contact_email",
                "contact_phone1",
                "contact_phone2",
                "contact_phone3",
                "contact_phone4",
                "contact_ownerName",
                "contact_contactOwner",
                "contact_ownerEmail",
                "contact_ownerPhone1",
                "contact_ownerPhone2",
                "contact_ownerPhone3",
                "contact_ownerPhone4"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "car_model"
            ],
            "meta": {
                "dictionaryParent": "car_make"
            }
        },
        {
            "targets": [
                "car_vin",
                "car_seller",
                "car_make",
                "car_model",
                "car_plateNum",
                "car_color",
                "car_transmission",
                "car_engine",
                "car_liters",
                "car_capacity",
                "car_dims",
                "car_weight",
                "car_checkPeriod",
                "car_class",
                "car_buyDate",
                "car_mileage",
                "car_checkupDate",
                "car_checkupMileage",
                "car_dealerTO",
                "car_makeYear"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "cardNumber_validFrom",
                "cardNumber_validUntil",
                "cardNumber_validUntilMilage",
                "cardNumber_milageTO",
                "cardNumber_serviceInterval",
                "cardNumber_cardOwner",
                "cardNumber_manager"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "car_plateNum"
            ],
            "meta": {
                "mainToo": true,
                "infoText": "platenum"
            }
        },
        {
            "targets": [
                "contact_contactOwner"
            ],
            "meta": {
                "mainToo": true,
                "infoText": "owner"
            }
        }
    ],
    "fields": [
        {
            "name": "callDate",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "index": true,
            "indexCollate": true,
            "type": "datetime",
            "meta": {
                "label": "Дата звонка",
		"readonly": true
            }
        },
        {
            "name": "callTaker",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "meta": {
                "label": "Сотрудник РАМК",
                "required": true,
                "readonly": true
            }
        },
        {
            "name": "comment",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Wazzup",
                "label": "Что случилось",
                "required": true,
                "infoText": "comment"
            }
        },
        {
            "name": "diagnosis1",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis1",
                "label": "Система",
                "required": true,
                "infoText": "system"
            }
        },
        {
            "name": "diagnosis2",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis2",
                "dictionaryParent": "diagnosis1",
                "label": "Узел/деталь",
                "infoText": "detail"
            }
        },
        {
            "name": "diagnosis3",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis3",
                "dictionaryParent": "diagnosis2",
                "label": "Описание причины неисправности",
                "infoText": "diagnosis3"
            }
        },
        {
            "name": "diagnosis4",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis4",
                "label": "Рекомендация",
                "infoText": "recomendation"
            }
        },
        {
            "name": "contact",
            "groupName": "carContact",
            "meta": {
                "label": "Клиент",
                "required": true
            }
        },
        {
            "name": "program",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "index": true,
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Programs",
                "label": "Программа",
                "required": true,
                "bounded":true,
                "targetCategory": "program",
                "infoText": "program"
            }
        },
        {
            "name": "car",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "groupName": "car"
        },
        {
            "name": "cardNumber",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin",
				"programman"
            ],
            "groupName": "cardNumber"
        },
        {
            "name": "vinChecked",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "VINChecked",
                "label": "Участие в программе",
                "required": true,
                "infoText": "vinChecked"
            }
        },
        {
            "name": "caseAddress",
            "groupName": "address"
        },
        {
            "name": "city",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "DealerCities",
                "label": "Город",
                "required": true,
                "infoText": "city"
            }
        },
        {
            "name": "temperature",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Температура",
                "infoText": "temperature"
            }
        },
        {
            "name": "dealerCause",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Причина неисправности со слов дилера",
                "infoText": "dealerCause"
            },
            "type": "textarea"
        },
        {
            "name": "caseStatus",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
            ],
            "type": "dictionary",
            "meta": {
                "required": true,
                "dictionaryName": "CaseStatuses",
                "label": "Статус кейса"
            }
        },
        {
            "name": "betaComment",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "meta": {
                "label": "Комментарии",
                "infoText": "betaComment"
            },
            "type": "textarea"
        },
        {
            "name": "services",
            "canRead": true,
            "canWrite": true,
            "type": "reference",
            "meta": {
                "label": "Услуги"
            }
        },
        {
            "name": "actions",
            "canRead": true,
            "canWrite": true,
            "type": "reference",
            "meta": {
                "label": "Действия",
                "invisible": true
            }
        },
        {
            "name": "files",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "files",
            "meta": {
                "label": "Прикрепленные файлы"
            }
        },
        {
            "name": "comments",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "json",
            "meta": {
                "invisible": true
            }
        }
    ]
}
