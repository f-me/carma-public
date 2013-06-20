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
            "meta": {
                "targetMap": "caseAddress_map",
                "targetCoords": "caseAddress_coords"
            }
        },
        {
            "targets": [
                "caseAddress_coords"
            ],
            "meta": {
                "targetMap": "caseAddress_map",
                "targetAddr": "caseAddress_address"
            }
        },
        {
            "targets": [
                "caseAddress_map"
            ],
            "meta": {
                "targetAddr": "caseAddress_address",
                "targetCoords": "caseAddress_coords",
                "cityField": "city"
            }
        },
        {
            "targets": [
                "caseAddress_address"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Карта участника",
                "infoText": "cardnum"
            }
        },
        {
            "targets": [
                "caseAddress_map",
                "caseAddress_coords",
                "caseAddress_city",
                "caseAddress_comment"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "car_makeYear",
                "car_warrantyStart",
                "car_warrantyEnd",
                "car_contractType"                
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
            "name": "vwcreatedate",
            "canRead": [
                "vwfake"
            ],
            "canWrite": [
                "vwfake"
            ],
            "index": true,
            "indexCollate": true,
            "type": "datetime",
            "meta": {
                "label": "Дата звонка"
            }
        },
        {
            "name": "callTaker",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "meta": {
                "label": "Сотрудник РАМК",
                "readonly": true
            }
        },
        {
            "name": "comment",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
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
                "head", "supervisor", "director", "analyst", "vwfake", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "account", "admin", "programman"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis3",
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
                "head", "supervisor", "director", "analyst", "vwfake", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "account", "admin", "programman"
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "VINChecked",
                "label": "Участие в программе",
                "required": true,
                "bounded": true,
                "infoText": "vinChecked"
            }
        },
        {
            "name": "city",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "DealerCities",
                "label": "Город",
                "required": true,
                "bounded": true,
                "infoText": "city"
            }
        },
        {
            "name": "caseAddress",
            "groupName": "address"
        },
        {
            "name": "temperature",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Температура",
                "infoText": "temperature"
            }
        },
                {
            "name": "repair",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "type": "datetime",
            "meta": {
                "label": "Дата починки"
            }
        },
        {
            "name": "accord",
            "canRead": [
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Номер согласования"
            }
        },
        {
            "name": "dealerCause",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Неисправность со слов дилера/партнёра",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy", "psaanalyst"
            ],
            "canWrite": [
                "psaanalyst",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin"
            ],
            "type": "dictionary",
            "meta": {
                "required": true,
                "bounded": true,
                "dictionaryName": "CaseStatuses",
                "label": "Статус кейса"
            }
        },
        {
            "name": "psaExportNeeded",
            "type": "checkbox",
            "canRead": [
                "head", "supervisor", "director", "admin", "psaanalyst"
            ],
            "canWrite": [
                "head", "supervisor", "director", "admin", "psaanalyst"
            ],
            "meta": {
                "label": "Требуется выгрузка в PSA"
            }
        },
        {
            "name": "psaExported",
            "type": "checkbox",
            "canRead": [
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman", "psaanalyst"
            ],
            "canWrite": [
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman"
            ],
            "meta": {
                "readonly": true,
                "label": "Выгружен в PSA"
            }
        },
        {
            "name": "claim",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "meta": {
                "label": "Претензия / Благодарность",
                "infoText": "claim"
            },
            "type": "textarea"
        },
        {
            "name": "betaComment",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "reference",
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
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "json",
            "meta": {
                "invisible": true
            }
        }
    ]
}
