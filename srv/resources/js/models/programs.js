{
  "programs": [
    {
      "name": "VW",
      "subprograms": [
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "DCapproved",
                          "type": "checkbox",
                          "label": "Клиент договорился с ДЦ"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <125 км"
                        },
                        {
                          "name": "nonAccident",
                          "type": "checkbox",
                          "label": "Не ДТП"
                        },
                        {
                          "name": "nonVandal",
                          "type": "checkbox",
                          "label": "Не вандализм"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TowageCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Техпомощь": [
                {
                  "Замена колеса": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "moreThan1Wheel",
                          "type": "checkbox",
                          "label": "Более одного колеса"
                        },
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "contractor",
                          "label": "Подрядчик",
                          "subform": "Contractor"
                        },
                        {
                          "name": "closeContractorsPresent",
                          "type": "checkbox",
                          "label": "Есть мастерские на расстоянии <125 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechSelectedCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Техпомощь": [
                {
                  "Зарядка АКБ": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "contractor",
                          "label": "Подрядчик",
                          "subform": "Contractor"
                        },
                        {
                          "name": "closeContractorsPresent",
                          "type": "checkbox",
                          "label": "Есть мастерские на расстоянии <125 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechSelectedCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "name": "",
              "services": [
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "otherServiceNotUsed",
                          "type": "checkbox",
                          "label": "Клиент не пользовался услугой Гостиница"
                        },
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "SelfTow",
                          "type": "checkbox",
                          "label": "Клиент самостоятельно добрался до дилера"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "dealerApprovedTow",
                          "type": "checkbox",
                          "label": "Дилер подтвердил самостоятельную эвакуацию клиента"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "name": "",
              "services": [
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "otherServiceNotUsed",
                          "type": "checkbox",
                          "label": "Клиент не пользовался услугой Подменный автомобиль"
                        },
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "SelfTow",
                          "type": "checkbox",
                          "label": "Клиент самостоятельно добрался до дилера"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "HotelCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Легковые автомобили",
          "conditions": [
            {
              "dateSelector": true,
              "name": "sellDate",
              "label": "Дата продажи"
            },
            {
              "name": "sellDateCheck1",
              "type": "checkbox",
              "label": "Дата продажи > 15.01.2010"
            },
            {
              "name": "sellDateCheck2",
              "type": "checkbox",
              "label": "Дата продажи < 2 лет"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ],
          "info": "Описание программы в виде текста"
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <250 км"
                        },
                        {
                          "name": "nonAccident",
                          "type": "checkbox",
                          "label": "Не ДТП"
                        },
                        {
                          "name": "nonVandal",
                          "type": "checkbox",
                          "label": "Не вандализм"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TowageCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Техпомощь": [
                {
                  "Замена колеса": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "moreThan1Wheel",
                          "type": "checkbox",
                          "label": "Более одного колеса"
                        },
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "contractor",
                          "label": "Подрядчик",
                          "subform": "Contractor"
                        },
                        {
                          "name": "closeContractorsPresent",
                          "type": "checkbox",
                          "label": "Есть мастерские на расстоянии <250 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechSelectedCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Техпомощь": [
                {
                  "Зарядка АКБ": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "contractor",
                          "label": "Подрядчик",
                          "subform": "Contractor"
                        },
                        {
                          "name": "closeContractorsPresent",
                          "type": "checkbox",
                          "label": "Есть мастерские на расстоянии <250 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechSelectedCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "name": "",
              "services": [
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "otherServiceNotUsed",
                          "type": "checkbox",
                          "label": "Клиент не пользовался услугой Гостиница"
                        },
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "SelfTow",
                          "type": "checkbox",
                          "label": "Клиент самостоятельно добрался до дилера"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "dealerApprovedTow",
                          "type": "checkbox",
                          "label": "Дилер подтвердил самостоятельную эвакуацию клиента"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "name": "",
              "services": [
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "otherServiceNotUsed",
                          "type": "checkbox",
                          "label": "Клиент не пользовался услугой Подменный автомобиль"
                        },
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "SelfTow",
                          "type": "checkbox",
                          "label": "Клиент самостоятельно добрался до дилера"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "HotelCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Коммерческие автомобили",
          "conditions": [
            {
              "name": "sellDate",
              "label": "Дата продажи",
              "datepicker": true
            },
            {
              "name": "sellDateCheck1",
              "type": "checkbox",
              "label": "Дата продажи > 01.06.2010"
            },
            {
              "name": "sellDateCheck2",
              "type": "checkbox",
              "label": "Дата продажи < 2 лет"
            },
            {
              "name": "make",
              "data": "CarMarkers",
              "label": "Марка"
            },
            {
              "name": "model",
              "data": "CarModels",
              "label": "Модель"
            },
            {
              "name": "approvedModels",
              "type": "textarea",
              "label": "Список моделей входящих в программу"
            },
            {
              "name": "modelApproved",
              "type": "checkbox",
              "label": "Модель входит в программу"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ],
          "info": "условия программы"
        }
      ]
    },
    {
      "name": "ACTA",
      "subprograms": [
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "Путешествие": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Bentley",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании ACTA"
            },
            {
              "name": "attachedFiles",
              "type": "files",
              "label": "Приложенные файлы"
            },
            {
              "name": "GOPApproved",
              "type": "checkbox",
              "label": "GOP предоставлена"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Техпомощь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "Путешествие": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Aston Martin",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании ACTA"
            },
            {
              "name": "attachedFiles",
              "type": "files",
              "label": "Приложенные файлы"
            },
            {
              "name": "GOPApproved",
              "type": "checkbox",
              "label": "GOP предоставлена"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        }
      ]
    },
    {
      "name": "B2B",
      "subprograms": [
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Транспортировка": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SparesCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "NotificationCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SoberDriverCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Arc B2B",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании Arc"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Транспортировка": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SparesCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "NotificationCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SoberDriverCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "RTR Hyundai",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании RTR Hyundai"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Транспортировка": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SparesCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "NotificationCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SoberDriverCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Дженсер Ясенево",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании Дженсер Ясенево"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Транспортировка": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SparesCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "NotificationCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SoberDriverCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "ИП Трубкин",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании ИП Трубкин"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Транспортировка": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SparesCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "NotificationCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SoberDriverCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "ДЦ Автоимпорт",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании ДЦ Автоимпорт"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Транспортировка": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SparesCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "NotificationCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SoberDriverCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "ДЦ ТВД-Авто",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании ДЦ ТВД-Авто"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "АРВАЛ",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании АРВАЛ"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Транспортировка": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportationCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "TransportDeliveryCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SparesCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "NotificationCommon.fields"
                    ]
                  }
                },
                {
                  "Информирование о происшествии": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      "SoberDriverCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "3S Телематика",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании 3S Телематика"
            },
            {
              "name": "companyEmployees",
              "label": "Контактные лица",
              "subform": "Employees"
            },
            {
              "name": "employeeApproved",
              "type": "checkbox",
              "label": "Сотрудник авторизован"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        }
      ]
    },
    {
      "name": "GM",
      "subprograms": [
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "DCapproved",
                          "type": "checkbox",
                          "label": "Клиент договорился с ДЦ"
                        },
                        {
                          "name": "nonAccident",
                          "type": "checkbox",
                          "label": "Не ДТП"
                        },
                        {
                          "name": "nonVandal",
                          "type": "checkbox",
                          "label": "Не вандализм"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "clientAddress",
                          "label": "Место жительства",
                          "subform": "Address"
                        },
                        {
                          "name": "distanceApproved",
                          "type": "checkbox",
                          "label": "Неисправность возникла на расстоянии 130 км от дома"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "parts",
                          "type": "textarea",
                          "label": "Запчасти"
                        },
                        {
                          "name": "partsToAddress",
                          "label": "Куда доставить",
                          "subform": "Address"
                        }
                      ],
                      "SparesCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "Путешествие": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "attachedFiles",
                          "type": "files",
                          "label": "Приложенные файлы"
                        },
                        {
                          "name": "TravelApproved",
                          "type": "checkbox",
                          "label": "Подтверждение путешествия предоставлено"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка к ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Chevrolet Korea",
          "conditions": [
            {
              "name": "sellDate",
              "label": "Дата продажи",
              "datepicker": true
            },
            {
              "name": "sellDateCheck1",
              "type": "checkbox",
              "label": "Дата продажи > 15.12.2010"
            },
            {
              "name": "sellDateCheck2",
              "type": "checkbox",
              "label": "Дата продажи < 2 лет"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "DCapproved",
                          "type": "checkbox",
                          "label": "Клиент договорился с ДЦ"
                        },
                        {
                          "name": "nonAccident",
                          "type": "checkbox",
                          "label": "Не ДТП"
                        },
                        {
                          "name": "nonVandal",
                          "type": "checkbox",
                          "label": "Не вандализм"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "clientAddress",
                          "label": "Место жительства",
                          "subform": "Address"
                        },
                        {
                          "name": "distanceApproved",
                          "type": "checkbox",
                          "label": "Неисправность возникла на расстоянии 130 км от дома"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "parts",
                          "type": "textarea",
                          "label": "Запчасти"
                        },
                        {
                          "name": "partsToAddress",
                          "label": "Куда доставить",
                          "subform": "Address"
                        }
                      ],
                      "SparesCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "Путешествие": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "attachedFiles",
                          "type": "files",
                          "label": "Приложенные файлы"
                        },
                        {
                          "name": "TravelApproved",
                          "type": "checkbox",
                          "label": "Подтверждение путешествия предоставлено"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка к ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Chevrolet NA",
          "conditions": [
            {
              "name": "sellDate",
              "label": "Дата продажи",
              "datepicker": true
            },
            {
              "name": "sellDateCheck1",
              "type": "checkbox",
              "label": "Дата продажи > 15.12.2010"
            },
            {
              "name": "sellDateCheck2",
              "type": "checkbox",
              "label": "Дата продажи < 2 лет"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "DCapproved",
                          "type": "checkbox",
                          "label": "Клиент договорился с ДЦ"
                        },
                        {
                          "name": "nonAccident",
                          "type": "checkbox",
                          "label": "Не ДТП"
                        },
                        {
                          "name": "nonVandal",
                          "type": "checkbox",
                          "label": "Не вандализм"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "clientAddress",
                          "label": "Место жительства",
                          "subform": "Address"
                        },
                        {
                          "name": "distanceApproved",
                          "type": "checkbox",
                          "label": "Неисправность возникла на расстоянии 130 км от дома"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "parts",
                          "type": "textarea",
                          "label": "Запчасти"
                        },
                        {
                          "name": "partsToAddress",
                          "label": "Куда доставить",
                          "subform": "Address"
                        }
                      ],
                      "SparesCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "Путешествие": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "attachedFiles",
                          "type": "files",
                          "label": "Приложенные файлы"
                        },
                        {
                          "name": "TravelApproved",
                          "type": "checkbox",
                          "label": "Подтверждение путешествия предоставлено"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка к ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Cadillac до 2012",
          "conditions": [
            {
              "name": "sellDate",
              "label": "Дата продажи",
              "datepicker": true
            },
            {
              "name": "sellDateCheck1",
              "type": "checkbox",
              "label": "Дата продажи > 01.02.2010"
            },
            {
              "name": "sellDateCheck2",
              "type": "checkbox",
              "label": "Дата продажи < 1 года"
            },
            {
              "name": "make",
              "data": "CarMarkers",
              "label": "Марка"
            },
            {
              "name": "model",
              "data": "CarModels",
              "label": "Модель"
            },
            {
              "name": "approvedModels",
              "type": "textarea",
              "label": "Список моделей входящих в программу"
            },
            {
              "name": "modelApproved",
              "type": "checkbox",
              "label": "Модель входит в программу"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "DCapproved",
                          "type": "checkbox",
                          "label": "Клиент договорился с ДЦ"
                        },
                        {
                          "name": "nonAccident",
                          "type": "checkbox",
                          "label": "Не ДТП"
                        },
                        {
                          "name": "nonVandal",
                          "type": "checkbox",
                          "label": "Не вандализм"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "clientAddress",
                          "label": "Место жительства",
                          "subform": "Address"
                        },
                        {
                          "name": "distanceApproved",
                          "type": "checkbox",
                          "label": "Неисправность возникла на расстоянии 130 км от дома"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "parts",
                          "type": "textarea",
                          "label": "Запчасти"
                        },
                        {
                          "name": "partsToAddress",
                          "label": "Куда доставить",
                          "subform": "Address"
                        }
                      ],
                      "SparesCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "Путешествие": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "attachedFiles",
                          "type": "files",
                          "label": "Приложенные файлы"
                        },
                        {
                          "name": "TravelApproved",
                          "type": "checkbox",
                          "label": "Подтверждение путешествия предоставлено"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка к ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Opel (после 01.04.2011]",
          "conditions": [
            {
              "name": "sellDate",
              "label": "Дата продажи",
              "datepicker": true
            },
            {
              "name": "sellDateCheck1",
              "type": "checkbox",
              "label": "Дата продажи > 01.02.2010"
            },
            {
              "name": "sellDateCheck2",
              "type": "checkbox",
              "label": "Дата продажи < 1 года"
            },
            {
              "name": "make",
              "data": "CarMarkers",
              "label": "Марка"
            },
            {
              "name": "model",
              "data": "CarModels",
              "label": "Модель"
            },
            {
              "name": "approvedModels",
              "type": "textarea",
              "label": "Список моделей входящих в программу"
            },
            {
              "name": "modelApproved",
              "type": "checkbox",
              "label": "Модель входит в программу"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "DCapproved",
                          "type": "checkbox",
                          "label": "Клиент договорился с ДЦ"
                        },
                        {
                          "name": "nonAccident",
                          "type": "checkbox",
                          "label": "Не ДТП"
                        },
                        {
                          "name": "nonVandal",
                          "type": "checkbox",
                          "label": "Не вандализм"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "clientAddress",
                          "label": "Место жительства",
                          "subform": "Address"
                        },
                        {
                          "name": "distanceApproved",
                          "type": "checkbox",
                          "label": "Неисправность возникла на расстоянии 130 км от дома"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "parts",
                          "type": "textarea",
                          "label": "Запчасти"
                        },
                        {
                          "name": "partsToAddress",
                          "label": "Куда доставить",
                          "subform": "Address"
                        }
                      ],
                      "SparesCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "Путешествие": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "attachedFiles",
                          "type": "files",
                          "label": "Приложенные файлы"
                        },
                        {
                          "name": "TravelApproved",
                          "type": "checkbox",
                          "label": "Подтверждение путешествия предоставлено"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка к ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Hummer",
          "conditions": [
            {
              "name": "sellDate",
              "label": "Дата продажи",
              "datepicker": true
            },
            {
              "name": "sellDateCheck1",
              "type": "checkbox",
              "label": "Дата продажи > 01.04.2011"
            },
            {
              "name": "sellDateCheck2",
              "type": "checkbox",
              "label": "Дата продажи < 3 года"
            },
            {
              "name": "make",
              "data": "CarMarkers",
              "label": "Марка"
            },
            {
              "name": "model",
              "data": "CarModels",
              "label": "Модель"
            },
            {
              "name": "approvedModels",
              "type": "textarea",
              "label": "Список моделей входящих в программу"
            },
            {
              "name": "modelApproved",
              "type": "checkbox",
              "label": "Модель входит в программу"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "Техническая помошь": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechCommon.fields"
                    ]
                  }
                },
                {
                  "Эвакуация": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "closeDealersPresent",
                          "type": "checkbox",
                          "label": "Есть дилеры на расстоянии <130 км"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "DCapproved",
                          "type": "checkbox",
                          "label": "Клиент договорился с ДЦ"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TowageCommon.fields"
                    ]
                  }
                },
                {
                  "Подменный автомобиль": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "ReplacementVehicleCommon.fields"
                    ]
                  }
                },
                {
                  "Гостиница": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "clientAddress",
                          "label": "Место жительства",
                          "subform": "Address"
                        },
                        {
                          "name": "distanceApproved",
                          "type": "checkbox",
                          "label": "Неисправность возникла на расстоянии 130 км от дома"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "HotelCommon.fields"
                    ]
                  }
                },
                {
                  "Такси": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TaxiCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка запчастей": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "parts",
                          "type": "textarea",
                          "label": "Запчасти"
                        },
                        {
                          "name": "partsToAddress",
                          "label": "Куда доставить",
                          "subform": "Address"
                        }
                      ],
                      "SparesCommon.fields"
                    ]
                  }
                },
                {
                  "Трезвый водитель": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "serviceProvided",
                          "label": "Сколько раз предоставлена услуга"
                        },
                        {
                          "name": "serviceRestriction",
                          "label": "Не более трёх раз за срок действия программы"
                        },
                        {
                          "name": "restrictionApproved",
                          "type": "checkbox",
                          "label": "Лимит не исчерпан"
                        },
                        {
                          "name": "fromAddress",
                          "label": "Откуда",
                          "subform": "Address"
                        },
                        {
                          "name": "toAddress",
                          "label": "Куда доставить",
                          "subform": "Address"
                        },
                        {
                          "name": "distanceFromToApproved",
                          "type": "checkbox",
                          "label": "Расстояние <130 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "SoberDriverCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "Путешествие": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "attachedFiles",
                          "type": "files",
                          "label": "Приложенные файлы"
                        },
                        {
                          "name": "TravelApproved",
                          "type": "checkbox",
                          "label": "Подтверждение путешествия предоставлено"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                },
                {
                  "Доставка к ТС": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "RAMCtow",
                          "type": "checkbox",
                          "label": "Эвакуация РАМК"
                        },
                        {
                          "name": "towDealer",
                          "label": "Дилер",
                          "subform": "Dealer"
                        },
                        {
                          "name": "startRepairDate",
                          "label": "Дата начала ремонта",
                          "datepicker": true
                        },
                        {
                          "name": "plannedRepairDate",
                          "label": "Предполагаемая дата исправления автомобиля",
                          "datepicker": true
                        },
                        {
                          "name": "longRepair",
                          "type": "checkbox",
                          "label": "Неисправность не может быть исправлена в день обращения"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TransportationSelectedCommon.fields"
                    ]
                  }
                }
              ]
            },
            {
              "Техпомощь": [
                {
                  "Доставка топлива": {
                    "fieldgroup": [
                      "ServiceCommon.fields",
                      [
                        {
                          "name": "caseAddress",
                          "label": "Адрес кейса",
                          "subform": "Address"
                        },
                        {
                          "name": "contractor",
                          "label": "Подрядчик",
                          "subform": "Contractor"
                        },
                        {
                          "name": "closeContractorsPresent",
                          "type": "checkbox",
                          "label": "Есть мастерские на расстоянии <130 км"
                        },
                        {
                          "name": "serviceApproved",
                          "type": "checkbox",
                          "label": "Услуга может быть оказана по программе"
                        }
                      ],
                      "TechSelectedCommon.fields"
                    ]
                  }
                }
              ]
            }
          ],
          "name": "Caddilac после 2012",
          "conditions": [
            {
              "name": "sellDate",
              "label": "Дата продажи",
              "datepicker": true
            },
            {
              "name": "sellDateCheck1",
              "type": "checkbox",
              "label": "Дата продажи > 01.01.2012"
            },
            {
              "name": "sellDateCheck2",
              "type": "checkbox",
              "label": "Дата продажи < 3 года"
            },
            {
              "name": "make",
              "data": "CarMarkers",
              "label": "Марка"
            },
            {
              "name": "model",
              "data": "CarModels",
              "label": "Модель"
            },
            {
              "name": "approvedModels",
              "type": "textarea",
              "label": "Список моделей входящих в программу"
            },
            {
              "name": "modelApproved",
              "type": "checkbox",
              "label": "Модель входит в программу"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        }
      ]
    },
    {
      "name": "Другие",
      "subprograms": [
        {
          "Заказ билетов": {}
        },
        {
          "B2C": {
            "serviceGroups": [
              {
                "name": "",
                "services": [
                  {
                    "Эвакуация": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "serviceProvided": {
                              "label": "Сколько раз услуга Эвакуация предоставлена"
                            }
                          },
                          {
                            "serviceRestriction": {
                              "label": "Ограничение по количеству раз предоставления услуги"
                            }
                          },
                          {
                            "restrictionApproved": {
                              "type": "checkbox",
                              "label": "Лимит не исчерпан"
                            }
                          },
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceApproved": {
                              "type": "checkbox",
                              "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TowageCommon.fields"
                      ]
                    }
                  }
                ]
              },
              {
                "Техпомощь": [
                  {
                    "Подвоз топлива": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "serviceProvided": {
                              "label": "Сколько раз услуга Техпомощь - Подвоз топлива предоставлена"
                            }
                          },
                          {
                            "serviceRestriction": {
                              "label": "Ограничение по количеству раз предоставления услуги"
                            }
                          },
                          {
                            "restrictionApproved": {
                              "type": "checkbox",
                              "label": "Лимит не исчерпан"
                            }
                          },
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceApproved": {
                              "type": "checkbox",
                              "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechSelectedCommon.fields"
                      ]
                    }
                  },
                  {
                    "Запуск двигателя": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "serviceProvided": {
                              "label": "Сколько раз услуга Техпомощь - Запуск двигателя предоставлена"
                            }
                          },
                          {
                            "serviceRestriction": {
                              "label": "Ограничение по количеству раз предоставления услуги"
                            }
                          },
                          {
                            "restrictionApproved": {
                              "type": "checkbox",
                              "label": "Лимит не исчерпан"
                            }
                          },
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceApproved": {
                              "type": "checkbox",
                              "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechSelectedCommon.fields"
                      ]
                    }
                  },
                  {
                    "Замена колеса": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "serviceProvided": {
                              "label": "Сколько раз услуга Техпомощь - Замена колеса предоставлена"
                            }
                          },
                          {
                            "serviceRestriction": {
                              "label": "Ограничение по количеству раз предоставления услуги"
                            }
                          },
                          {
                            "restrictionApproved": {
                              "type": "checkbox",
                              "label": "Лимит не исчерпан"
                            }
                          },
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceApproved": {
                              "type": "checkbox",
                              "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechSelectedCommon.fields"
                      ]
                    }
                  },
                  {
                    "Вскрытие автомобиля": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "serviceProvided": {
                              "label": "Сколько раз услуга Техпомощь - Вскрытие автомобиля предоставлена"
                            }
                          },
                          {
                            "serviceRestriction": {
                              "label": "Ограничение по количеству раз предоставления услуги"
                            }
                          },
                          {
                            "restrictionApproved": {
                              "type": "checkbox",
                              "label": "Лимит не исчерпан"
                            }
                          },
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceApproved": {
                              "type": "checkbox",
                              "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechSelectedCommon.fields"
                      ]
                    }
                  }
                ]
              }
            ],
            "conditions": [
              {
                "memberNumber": {
                  "label": "Карта участника"
                }
              },
              {
                "memberApproved": {
                  "type": "checkbox",
                  "label": "Участник программы"
                }
              },
              {
                "isMember": {
                  "type": "checkbox",
                  "label": "Клиент участвует в программе"
                }
              }
            ]
          }
        },
        {
          "Ford": {
            "serviceGroups": [
              {
                "name": "",
                "services": [
                  {
                    "Эвакуация": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "closeDealersPresent": {
                              "type": "checkbox",
                              "label": "Есть дилеры на расстоянии <200 км"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TowageCommon.fields"
                      ]
                    }
                  },
                  {
                    "Техническая помощь": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "contractor": {
                              "label": "Подрядчик",
                              "subform": "Contractor"
                            }
                          },
                          {
                            "closeContractorsPresent": {
                              "type": "checkbox",
                              "label": "Есть мастерские на расстоянии <200 км"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechCommon.fields"
                      ]
                    }
                  },
                  {
                    "Информирование о происшествии": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "RAMCtow": {
                              "type": "checkbox",
                              "label": "Эвакуация РАМК"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "NotificationCommon.fields"
                      ]
                    }
                  },
                  {
                    "Подменный автомобиль": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "RAMCtow": {
                              "type": "checkbox",
                              "label": "Эвакуация РАМК"
                            }
                          },
                          {
                            "towDealer": {
                              "label": "Дилер",
                              "subform": "Dealer"
                            }
                          },
                          {
                            "startRepairDate": {
                              "label": "Дата начала ремонта",
                              "datepicker": true
                            }
                          },
                          {
                            "plannedRepairDate": {
                              "label": "Предполагаемая дата исправления автомобиля",
                              "datepicker": true
                            }
                          },
                          {
                            "longRepair": {
                              "type": "checkbox",
                              "label": "Неисправность не может быть исправлена в день обращения"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "ReplacementVehicleCommon.fields"
                      ]
                    }
                  },
                  {
                    "Такси": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "RAMCtow": {
                              "type": "checkbox",
                              "label": "Эвакуация РАМК"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TaxiCommon.fields"
                      ]
                    }
                  }
                ]
              }
            ],
            "conditions": [
              {
                "VINApproved": {
                  "type": "checkbox",
                  "label": "VIN в списке участников программы"
                }
              },
              {
                "lastTODate": {
                  "label": "Дата последнего ТО",
                  "datepicker": true
                }
              },
              {
                "TOApproved": {
                  "type": "checkbox",
                  "label": "Дата последнего ТО < 1 года"
                }
              },
              {
                "milageTO": {
                  "label": "Пробег на последнем ТО"
                }
              },
              {
                "milage": {
                  "label": "Пробег"
                }
              },
              {
                "milageApproved": {
                  "type": "checkbox",
                  "label": "Межсервисный интервал не пройден"
                }
              },
              {
                "isMember": {
                  "type": "checkbox",
                  "label": "Клиент участвует в программе"
                }
              }
            ]
          }
        },
        {
          "BP": {
            "serviceGroups": [
              {
                "name": "",
                "services": [
                  {
                    "Эвакуация": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        "TowageCommon.fields"
                      ]
                    }
                  }
                ]
              },
              {
                "Техпомощь": [
                  {
                    "Слив топлива": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        "TechSelectedCommon.fields"
                      ]
                    }
                  }
                ]
              }
            ],
            "conditions": [
              {
                "companyApproved": {
                  "type": "checkbox",
                  "label": "Обращение компании BP"
                }
              },
              {
                "isMember": {
                  "type": "checkbox",
                  "label": "Клиент участвует в программе"
                }
              }
            ]
          }
        },
        {
          "Рус Лан": {
            "serviceGroups": [
              {
                "name": "",
                "services": [
                  {
                    "Техпомощь": [
                      {
                        "Замена колеса": {
                          "fieldgroup": [
                            "ServiceCommon.fields",
                            [
                              {
                                "caseAddress": {
                                  "label": "Адрес кейса",
                                  "subform": "Address"
                                }
                              },
                              {
                                "contractor": {
                                  "label": "Подрядчик",
                                  "subform": "Contractor"
                                }
                              },
                              {
                                "closeContractorsPresent": {
                                  "type": "checkbox",
                                  "label": "Есть мастерские на расстоянии <125 км"
                                }
                              },
                              {
                                "serviceApproved": {
                                  "type": "checkbox",
                                  "label": "Услуга может быть оказана по программе"
                                }
                              }
                            ],
                            "TechSelectedCommon.fields"
                          ]
                        }
                      }
                    ]
                  },
                  {
                    "Техпомощь": [
                      {
                        "Зарядка АКБ": {
                          "fieldgroup": [
                            "ServiceCommon.fields",
                            [
                              {
                                "caseAddress": {
                                  "label": "Адрес кейса",
                                  "subform": "Address"
                                }
                              },
                              {
                                "contractor": {
                                  "label": "Подрядчик",
                                  "subform": "Contractor"
                                }
                              },
                              {
                                "closeContractorsPresent": {
                                  "type": "checkbox",
                                  "label": "Есть мастерские на расстоянии <125 км"
                                }
                              },
                              {
                                "serviceApproved": {
                                  "type": "checkbox",
                                  "label": "Услуга может быть оказана по программе"
                                }
                              }
                            ],
                            "TechSelectedCommon.fields"
                          ]
                        }
                      }
                    ]
                  },
                  {
                    "Техпомощь": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "contractor": {
                              "label": "Подрядчик",
                              "subform": "Contractor"
                            }
                          },
                          {
                            "closeContractorsPresent": {
                              "type": "checkbox",
                              "label": "Есть мастерские на расстоянии <125 км"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechCommon.fields"
                      ]
                    }
                  },
                  {
                    "Эвакуация": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "contractor": {
                              "label": "Подрядчик",
                              "subform": "Contractor"
                            }
                          },
                          {
                            "closeContractorsPresent": {
                              "type": "checkbox",
                              "label": "Есть мастерские на расстоянии < 125 км"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TowageCommon.fields"
                      ]
                    }
                  }
                ]
              }
            ],
            "conditions": [
              {
                "caseAddress": {
                  "label": "Адрес кейса",
                  "subform": "Address"
                }
              },
              {
                "contractor": {
                  "label": "Подрядчик",
                  "subform": "Contractor"
                }
              },
              {
                "closeContractorsPresent": {
                  "type": "checkbox",
                  "label": "Есть мастерские на расстоянии <125 км"
                }
              },
              {
                "serviceApproved": {
                  "type": "checkbox",
                  "label": "Услуга может быть оказана по программе"
                }
              }
            ]
          }
        },
        {
          "Атлант М": {
            "serviceGroups": [
              {
                "name": "",
                "services": [
                  {
                    "Техпомощь": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "contractor": {
                              "label": "Подрядчик",
                              "subform": "Contractor"
                            }
                          },
                          {
                            "closeContractorsPresent": {
                              "type": "checkbox",
                              "label": "Есть мастерские на расстоянии <125 км"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechCommon.fields"
                      ]
                    }
                  },
                  {
                    "Эвакуация": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "contractor": {
                              "label": "Подрядчик",
                              "subform": "Contractor"
                            }
                          },
                          {
                            "closeContractorsPresent": {
                              "type": "checkbox",
                              "label": "Есть мастерские на расстоянии < 125 км"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TowageCommon.fields"
                      ]
                    }
                  },
                  {
                    "Информирование о происшествии": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        "NotificationCommon.fields"
                      ]
                    }
                  }
                ]
              }
            ],
            "conditions": [
              {
                "VINApproved": {
                  "type": "checkbox",
                  "label": "VIN в списке участников программы"
                }
              },
              {
                "programEndDate": {
                  "label": "Срок действия программы",
                  "datepicker": true
                }
              },
              {
                "programNotExpired": {
                  "type": "checkbox",
                  "label": "Программа действует"
                }
              },
              {
                "programEndMilage": {
                  "label": "Ограничение по пробегу"
                }
              },
              {
                "milage": {
                  "label": "Пробег"
                }
              },
              {
                "milageApproved": {
                  "type": "checkbox",
                  "label": "Межсервисный интервал не пройден"
                }
              },
              {
                "isMember": {
                  "type": "checkbox",
                  "label": "Клиент участвует в программе"
                }
              }
            ]
          }
        },
        {
          "Chartis Assistance": {
            "serviceGroups": [
              {
                "name": "",
                "services": [
                  {
                    "Эвакуация": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceMskSpbApproved": {
                              "type": "checkbox",
                              "label": "Кейс на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                            }
                          },
                          {
                            "nonAccident": {
                              "type": "checkbox",
                              "label": "Не ДТП"
                            }
                          },
                          {
                            "nonVandal": {
                              "type": "checkbox",
                              "label": "Не вандализм"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TowageCommon.fields"
                      ]
                    }
                  },
                  {
                    "Подменный автомобиль": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "RAMCtow": {
                              "type": "checkbox",
                              "label": "Эвакуация РАМК"
                            }
                          },
                          {
                            "towDealer": {
                              "label": "Дилер",
                              "subform": "Dealer"
                            }
                          },
                          {
                            "startRepairDate": {
                              "label": "Дата начала ремонта",
                              "datepicker": true
                            }
                          },
                          {
                            "plannedRepairDate": {
                              "label": "Предполагаемая дата исправления автомобиля",
                              "datepicker": true
                            }
                          },
                          {
                            "longRepair": {
                              "type": "checkbox",
                              "label": "Неисправность не может быть исправлена в день обращения"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "ReplacementVehicleCommon.fields"
                      ]
                    }
                  },
                  {
                    "Такси": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "RAMCtow": {
                              "type": "checkbox",
                              "label": "Эвакуация РАМК"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TaxiCommon.fields"
                      ]
                    }
                  }
                ]
              },
              {
                "Техпомощь": [
                  {
                    "Зарядка АКБ": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceMskSpbApproved": {
                              "type": "checkbox",
                              "label": "Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechSelectedCommon.fields"
                      ]
                    }
                  },
                  {
                    "Замена колеса": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceMskSpbApproved": {
                              "type": "checkbox",
                              "label": "Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechSelectedCommon.fields"
                      ]
                    }
                  },
                  {
                    "Вскрытие автомобиля": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceMskSpbApproved": {
                              "type": "checkbox",
                              "label": "Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechSelectedCommon.fields"
                      ]
                    }
                  },
                  {
                    "Подвоз топлива": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceMskSpbApproved": {
                              "type": "checkbox",
                              "label": "Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechSelectedCommon.fields"
                      ]
                    }
                  }
                ]
              },
              {
                "Транспортировка": [
                  {
                    "Путешествие": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "RAMCtow": {
                              "type": "checkbox",
                              "label": "Эвакуация РАМК"
                            }
                          },
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "clientAddress": {
                              "label": "Место жительства",
                              "subform": "Address"
                            }
                          },
                          {
                            "towDealer": {
                              "label": "Дилер",
                              "subform": "Dealer"
                            }
                          },
                          {
                            "dealerClientDistanceApproved": {
                              "type": "checkbox",
                              "label": "Дилер в 100 км от местра проживания клиента"
                            }
                          },
                          {
                            "startRepairDate": {
                              "label": "Дата начала ремонта",
                              "datepicker": true
                            }
                          },
                          {
                            "plannedRepairDate": {
                              "label": "Предполагаемая дата исправления автомобиля",
                              "datepicker": true
                            }
                          },
                          {
                            "longRepair": {
                              "type": "checkbox",
                              "label": "Неисправность не может быть исправлена в день обращения"
                            }
                          },
                          {
                            "attachedFiles": {
                              "type": "files",
                              "label": "Приложенные файлы"
                            }
                          },
                          {
                            "TravelApproved": {
                              "type": "checkbox",
                              "label": "Подтверждение путешествия предоставлено"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TransportationSelectedCommon.fields"
                      ]
                    }
                  },
                  {
                    "Доставка к ТС": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "RAMCtow": {
                              "type": "checkbox",
                              "label": "Эвакуация РАМК"
                            }
                          },
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "clientAddress": {
                              "label": "Место жительства",
                              "subform": "Address"
                            }
                          },
                          {
                            "towDealer": {
                              "label": "Дилер",
                              "subform": "Dealer"
                            }
                          },
                          {
                            "dealerClientDistanceApproved": {
                              "type": "checkbox",
                              "label": "Дилер в 100 км от местра проживания клиента"
                            }
                          },
                          {
                            "startRepairDate": {
                              "label": "Дата начала ремонта",
                              "datepicker": true
                            }
                          },
                          {
                            "plannedRepairDate": {
                              "label": "Предполагаемая дата исправления автомобиля",
                              "datepicker": true
                            }
                          },
                          {
                            "longRepair": {
                              "type": "checkbox",
                              "label": "Неисправность не может быть исправлена в день обращения"
                            }
                          },
                          {
                            "attachedFiles": {
                              "type": "files",
                              "label": "Приложенные файлы"
                            }
                          },
                          {
                            "TravelApproved": {
                              "type": "checkbox",
                              "label": "Подтверждение путешествия предоставлено"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TransportationSelectedCommon.fields"
                      ]
                    }
                  }
                ]
              }
            ],
            "conditions": [
              {
                "VINApproved": {
                  "type": "checkbox",
                  "label": "VIN в списке участников программы"
                }
              },
              {
                "isMember": {
                  "type": "checkbox",
                  "label": "Клиент участвует в программе"
                }
              }
            ]
          }
        },
        {
          "Autokraft Assistance": {
            "serviceGroups": [
              {
                "name": "",
                "services": [
                  {
                    "Эвакуация": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceMoscowApproved": {
                              "type": "checkbox",
                              "label": "Кейс на расстоянии < 100 км до Москвы"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TowageCommon.fields"
                      ]
                    }
                  },
                  {
                    "Техническая помощь": {
                      "fieldgroup": [
                        "ServiceCommon.fields",
                        [
                          {
                            "caseAddress": {
                              "label": "Адрес кейса",
                              "subform": "Address"
                            }
                          },
                          {
                            "distanceMoscowApproved": {
                              "type": "checkbox",
                              "label": "Кейс на расстоянии < 100 км до Москвы"
                            }
                          },
                          {
                            "serviceApproved": {
                              "type": "checkbox",
                              "label": "Услуга может быть оказана по программе"
                            }
                          }
                        ],
                        "TechCommon.fields"
                      ]
                    }
                  }
                ]
              }
            ],
            "conditions": [
              {
                "VINApproved": {
                  "type": "checkbox",
                  "label": "VIN в списке участников программы"
                }
              },
              {
                "programEndDate": {
                  "label": "Срок действия программы",
                  "datepicker": true
                }
              },
              {
                "programNotExpired": {
                  "type": "checkbox",
                  "label": "Программа действует"
                }
              },
              {
                "programEndMilage": {
                  "label": "Ограничение по пробегу"
                }
              },
              {
                "milage": {
                  "label": "Пробег"
                }
              },
              {
                "milageApproved": {
                  "type": "checkbox",
                  "label": "Межсервисный интервал не пройден"
                }
              },
              {
                "isMember": {
                  "type": "checkbox",
                  "label": "Клиент участвует в программе"
                }
              }
            ]
          }
        }
      ]
    }
  ],
  "commons": {
    "TaxiCommon": {
      "fields": [
        {
          "name": "taxiFrom",
          "label": "Откуда",
          "subform": "Address"
        },
        {
          "name": "taxiTo",
          "label": "Куда доставить",
          "subform": "Address"
        }
      ]
    },
    "HotelCommon": {
      "fields": [
        {
          "name": "caseAddress",
          "label": "Адрес кейса",
          "subform": "Address"
        }
      ]
    },
    "TechCommon": {
      "fields": [
        {
          "name": "techType",
          "data": "TechTypes",
          "label": "Тип техпомощи"
        },
        {
          "name": "caseAddress",
          "label": "Адрес кейса",
          "subform": "Address"
        },
        {
          "name": "techContractor",
          "label": "Подрядчик",
          "subform": "Contractor"
        },
        {
          "name": "techComments",
          "type": "textarea",
          "label": "Примечания"
        }
      ]
    },
    "TransportationCommon": {
      "fields": [
        {
          "name": "transportType",
          "data": "TransportTypes",
          "label": "Тип транспортировки"
        },
        {
          "name": "transportFrom",
          "label": "Откуда",
          "subform": "Address"
        },
        {
          "name": "transportTo",
          "label": "Куда доставить",
          "subform": "Address"
        }
      ]
    },
    "ReplacementVehicleCommon": {
      "fields": [
        {
          "name": "towDealer",
          "label": "Дилер",
          "subform": "Dealer"
        },
        {
          "name": "rentAddress",
          "label": "Куда доставить",
          "subform": "Address"
        },
        {
          "name": "carClass",
          "data": "CarClasses",
          "label": "Класс автомобиля"
        },
        {
          "name": "rentContractor",
          "label": "Подрядчик",
          "subform": "Contractor"
        }
      ]
    },
    "SparesCommon": {
      "fields": [
        {
          "name": "parts",
          "type": "textarea",
          "label": "Запчасти"
        },
        {
          "name": "partsToAddress",
          "label": "Куда доставить",
          "subform": "Address"
        }
      ]
    },
    "NotificationCommon": {
      "fields": [
        {
          "name": "infoContact1",
          "label": "Контакт 1"
        },
        {
          "name": "infoPhone1",
          "label": "Телефон 1"
        },
        {
          "name": "info1",
          "type": "textarea",
          "label": "Что сказать 1"
        },
        {
          "name": "infoContact2",
          "label": "Контакт 2"
        },
        {
          "name": "infoPhone2",
          "label": "Телефон 2"
        },
        {
          "name": "info2",
          "type": "textarea",
          "label": "Что сказать 2"
        },
        {
          "name": "infoContact3",
          "label": "Контакт 3"
        },
        {
          "name": "infoPhone3",
          "label": "Телефон 3"
        },
        {
          "name": "info3",
          "type": "textarea",
          "label": "Что сказать 3"
        }
      ]
    },
    "ServiceCommon": {
      "fields": [
        {
          "required": true,
          "name": "status",
          "data": "ServiceStatuses",
          "label": "Статус услуги"
        },
        {
          "name": "paymentType",
          "default": 0,
          "data": "PaymentTypes",
          "type": "options",
          "label": "Тип оплаты"
        },
        {
          "name": "cost",
          "label": "Стоимость"
        },
        {
          "datetime": true,
          "name": "requiredTime",
          "label": "Ожидаемое время оказания услуги"
        },
        {
          "name": "falseServices",
          "data": "FalseStatuses",
          "label": "Ложный вызов"
        },
        {
          "name": "caseAddress",
          "label": "Адрес кейса",
          "subform": "Address"
        }
      ]
    },
    "TechSelectedCommon": {
      "fields": [
        {
          "name": "caseAddress",
          "label": "Адрес кейса",
          "subform": "Address"
        },
        {
          "name": "techContractor",
          "label": "Подрядчик",
          "subform": "Contractor"
        },
        {
          "name": "techComments",
          "type": "textarea",
          "label": "Примечания"
        }
      ]
    },
    "TowageCommon": {
      "fields": [
        {
          "required": true,
          "name": "towerType",
          "data": "TowerTypes",
          "label": "Тип эвакуатора"
        },
        {
          "required": true,
          "name": "towType",
          "data": "TowTypes",
          "label": "Тип эвакуации"
        },
        {
          "name": "caseAddress",
          "label": "Адрес кейса",
          "subform": "Address"
        },
        {
          "name": "towDealer",
          "label": "Дилер",
          "subform": "Dealer"
        },
        {
          "name": "towAddress",
          "label": "Адрес доставки",
          "subform": "Address"
        },
        {
          "name": "towContractor",
          "label": "Подрядчик",
          "subform": "Contractor"
        },
        {
          "name": "wheelsUnblocked",
          "type": "checkbox",
          "label": "Колёса не заблокированы"
        },
        {
          "name": "manipulatorPossible",
          "type": "checkbox",
          "label": "Есть место для манипулятора"
        }
      ]
    },
    "TransportDeliveryCommon": {
      "fields": [
        {
          "name": "transportCarTo",
          "label": "Куда доставить",
          "subform": "Address"
        }
      ]
    },
    "SoberDriverCommon": {
      "fields": [
        {
          "name": "fromAddress",
          "label": "Откуда",
          "subform": "Address"
        },
        {
          "name": "toAddress",
          "label": "Куда доставить",
          "subform": "Address"
        },
        {
          "name": "multidrive",
          "type": "checkbox",
          "label": "Каско МУЛЬТИДРАЙВ"
        }
      ]
    },
    "TransportationSelectedCommon": {
      "fields": [
        {
          "name": "transportFrom",
          "label": "Откуда",
          "subform": "Address"
        },
        {
          "name": "transportTo",
          "label": "Куда доставить",
          "subform": "Address"
        }
      ]
    }
  }
}
