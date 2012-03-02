{
  "programClasses": [
    {
      "name": "VW",
      "programs": [
        {
          "name": "Легковые автомобили",
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                }
              ]
            },
            {
              "name": "Техпомощь",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechSelectedCommon"
                  ],
                  "name": "Замена колеса"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechSelectedCommon"
                  ],
                  "name": "Зарядка АКБ"
                }
              ]
            },
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                }
              ]
            },
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                }
              ]
            }
          ]
        },
        {
          "name": "Коммерческие автомобили",
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                }
              ]
            },
            {
              "name": "Техпомощь",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechSelectedCommon"
                  ],
                  "name": "Замена колеса"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechSelectedCommon"
                  ],
                  "name": "Зарядка АКБ"
                }
              ]
            },
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                }
              ]
            },
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                }
              ]
            }
          ],
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
      "programs": [
        {
          "name": "Bentley",
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                }
              ]
            },
            {
              "name": "Транспортировка",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                }
              ]
            }
          ],
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
          "name": "Aston Martin",
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техпомощь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                }
              ]
            },
            {
              "name": "Транспортировка",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                }
              ]
            }
          ],
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
      "programs": [
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationCommon"
                  ],
                  "name": "Транспортировка"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SoberDriverCommon"
                  ],
                  "name": "Информирование о происшествии"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationCommon"
                  ],
                  "name": "Транспортировка"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SoberDriverCommon"
                  ],
                  "name": "Информирование о происшествии"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationCommon"
                  ],
                  "name": "Транспортировка"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SoberDriverCommon"
                  ],
                  "name": "Информирование о происшествии"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationCommon"
                  ],
                  "name": "Транспортировка"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SoberDriverCommon"
                  ],
                  "name": "Информирование о происшествии"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationCommon"
                  ],
                  "name": "Транспортировка"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SoberDriverCommon"
                  ],
                  "name": "Информирование о происшествии"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationCommon"
                  ],
                  "name": "Транспортировка"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SoberDriverCommon"
                  ],
                  "name": "Информирование о происшествии"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportationCommon"
                  ],
                  "name": "Транспортировка"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TransportDeliveryCommon"
                  ],
                  "name": "Доставка ТС"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "SoberDriverCommon"
                  ],
                  "name": "Информирование о происшествии"
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
      "programs": [
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                }
              ]
            },
            {
              "name": "Транспортировка",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Доставка к ТС"
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
          "name": "Chevrolet NA",
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Доставка к ТС"
                }
              ]
            }
          ],
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
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Доставка к ТС"
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
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Доставка к ТС"
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
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "SoberDriverCommon"
                  ],
                  "name": "Трезвый водитель"
                }
              ]
            },
            {
              "name": "Транспортировка",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Доставка к ТС"
                }
              ]
            },
            {
              "name": "Техпомощь",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechSelectedCommon"
                  ],
                  "name": "Доставка топлива"
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
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechCommon"
                  ],
                  "name": "Техническая помошь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "HotelCommon"
                  ],
                  "name": "Гостиница"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "SparesCommon"
                  ],
                  "name": "Доставка запчастей"
                }
              ]
            },
            {
              "name": "Транспортировка",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Доставка к ТС"
                }
              ]
            }
          ],
          "name": "Cadillac после 2012",
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
      "programs": [
        {
          "serviceGroups": [],
          "name": "Заказ билетов"
        },
        {
          "name": "B2C",
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "serviceProvided",
                        "label": "Сколько раз услуга Эвакуация предоставлена"
                      },
                      {
                        "name": "serviceRestriction",
                        "label": "Ограничение по количеству раз предоставления услуги"
                      },
                      {
                        "name": "restrictionApproved",
                        "type": "checkbox",
                        "label": "Лимит не исчерпан"
                      },
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceApproved",
                        "type": "checkbox",
                        "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                }
              ]
            },
            {
              "Техпомощь": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "serviceProvided",
                        "label": "Сколько раз услуга Техпомощь - Подвоз топлива предоставлена"
                      },
                      {
                        "name": "serviceRestriction",
                        "label": "Ограничение по количеству раз предоставления услуги"
                      },
                      {
                        "name": "restrictionApproved",
                        "type": "checkbox",
                        "label": "Лимит не исчерпан"
                      },
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceApproved",
                        "type": "checkbox",
                        "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechSelectedCommon"
                  ],
                  "name": "Подвоз топлива"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "serviceProvided",
                        "label": "Сколько раз услуга Техпомощь - Запуск двигателя предоставлена"
                      },
                      {
                        "name": "serviceRestriction",
                        "label": "Ограничение по количеству раз предоставления услуги"
                      },
                      {
                        "name": "restrictionApproved",
                        "type": "checkbox",
                        "label": "Лимит не исчерпан"
                      },
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceApproved",
                        "type": "checkbox",
                        "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechSelectedCommon"
                  ],
                  "name": "Запуск двигателя"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "serviceProvided",
                        "label": "Сколько раз услуга Техпомощь - Замена колеса предоставлена"
                      },
                      {
                        "name": "serviceRestriction",
                        "label": "Ограничение по количеству раз предоставления услуги"
                      },
                      {
                        "name": "restrictionApproved",
                        "type": "checkbox",
                        "label": "Лимит не исчерпан"
                      },
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceApproved",
                        "type": "checkbox",
                        "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechSelectedCommon"
                  ],
                  "name": "Замена колеса"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "serviceProvided",
                        "label": "Сколько раз услуга Техпомощь - Вскрытие автомобиля предоставлена"
                      },
                      {
                        "name": "serviceRestriction",
                        "label": "Ограничение по количеству раз предоставления услуги"
                      },
                      {
                        "name": "restrictionApproved",
                        "type": "checkbox",
                        "label": "Лимит не исчерпан"
                      },
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceApproved",
                        "type": "checkbox",
                        "label": "Кейс на расстоянии < 50 км до Москвы или < 30 км до города"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechSelectedCommon"
                  ],
                  "name": "Вскрытие автомобиля"
                }
              ]
            }
          ],
          "conditions": [
            {
              "name": "memberNumber",
              "label": "Карта участника"
            },
            {
              "name": "memberApproved",
              "type": "checkbox",
              "label": "Участник программы"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "closeDealersPresent",
                        "type": "checkbox",
                        "label": "Есть дилеры на расстоянии <200 км"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                        "label": "Есть мастерские на расстоянии <200 км"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechCommon"
                  ],
                  "name": "Техническая помощь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                }
              ]
            }
          ],
          "name": "Ford",
          "conditions": [
            {
              "name": "VINApproved",
              "type": "checkbox",
              "label": "VIN в списке участников программы"
            },
            {
              "name": "lastTODate",
              "label": "Дата последнего ТО",
              "datepicker": true
            },
            {
              "name": "TOApproved",
              "type": "checkbox",
              "label": "Дата последнего ТО < 1 года"
            },
            {
              "name": "milageTO",
              "label": "Пробег на последнем ТО"
            },
            {
              "name": "milage",
              "label": "Пробег"
            },
            {
              "name": "milageApproved",
              "type": "checkbox",
              "label": "Межсервисный интервал не пройден"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                }
              ]
            },
            {
              "Техпомощь": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "TechSelectedCommon"
                  ],
                  "name": "Слив топлива"
                }
              ]
            }
          ],
          "name": "BP",
          "conditions": [
            {
              "name": "companyApproved",
              "type": "checkbox",
              "label": "Обращение компании BP"
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
                  "Техпомощь": [
                    {
                      "fieldgroup": [
                        "ServiceCommon",
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
                        "TechSelectedCommon"
                      ],
                      "name": "Замена колеса"
                    }
                  ]
                },
                {
                  "Техпомощь": [
                    {
                      "fieldgroup": [
                        "ServiceCommon",
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
                        "TechSelectedCommon"
                      ],
                      "name": "Зарядка АКБ"
                    }
                  ]
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechCommon"
                  ],
                  "name": "Техпомощь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                        "label": "Есть мастерские на расстоянии < 125 км"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                }
              ]
            }
          ],
          "name": "Рус Лан",
          "conditions": [
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
          ]
        },
        {
          "serviceGroups": [
            {
              "name": "",
              "services": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TechCommon"
                  ],
                  "name": "Техпомощь"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                        "label": "Есть мастерские на расстоянии < 125 км"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    "NotificationCommon"
                  ],
                  "name": "Информирование о происшествии"
                }
              ]
            }
          ],
          "name": "Атлант М",
          "conditions": [
            {
              "name": "VINApproved",
              "type": "checkbox",
              "label": "VIN в списке участников программы"
            },
            {
              "name": "programEndDate",
              "label": "Срок действия программы",
              "datepicker": true
            },
            {
              "name": "programNotExpired",
              "type": "checkbox",
              "label": "Программа действует"
            },
            {
              "name": "programEndMilage",
              "label": "Ограничение по пробегу"
            },
            {
              "name": "milage",
              "label": "Пробег"
            },
            {
              "name": "milageApproved",
              "type": "checkbox",
              "label": "Межсервисный интервал не пройден"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceMskSpbApproved",
                        "type": "checkbox",
                        "label": "Кейс на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
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
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "ReplacementVehicleCommon"
                  ],
                  "name": "Подменный автомобиль"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                    "TaxiCommon"
                  ],
                  "name": "Такси"
                }
              ]
            },
            {
              "Техпомощь": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceMskSpbApproved",
                        "type": "checkbox",
                        "label": "Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechSelectedCommon"
                  ],
                  "name": "Зарядка АКБ"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceMskSpbApproved",
                        "type": "checkbox",
                        "label": "Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechSelectedCommon"
                  ],
                  "name": "Замена колеса"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceMskSpbApproved",
                        "type": "checkbox",
                        "label": "Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechSelectedCommon"
                  ],
                  "name": "Вскрытие автомобиля"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceMskSpbApproved",
                        "type": "checkbox",
                        "label": "Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechSelectedCommon"
                  ],
                  "name": "Подвоз топлива"
                }
              ]
            },
            {
              "Транспортировка": [
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                        "name": "towDealer",
                        "label": "Дилер",
                        "subform": "Dealer"
                      },
                      {
                        "name": "dealerClientDistanceApproved",
                        "type": "checkbox",
                        "label": "Дилер в 100 км от местра проживания клиента"
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Путешествие"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
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
                        "name": "towDealer",
                        "label": "Дилер",
                        "subform": "Dealer"
                      },
                      {
                        "name": "dealerClientDistanceApproved",
                        "type": "checkbox",
                        "label": "Дилер в 100 км от местра проживания клиента"
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
                    "TransportationSelectedCommon"
                  ],
                  "name": "Доставка к ТС"
                }
              ]
            }
          ],
          "name": "Chartis Assistance",
          "conditions": [
            {
              "name": "VINApproved",
              "type": "checkbox",
              "label": "VIN в списке участников программы"
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
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceMoscowApproved",
                        "type": "checkbox",
                        "label": "Кейс на расстоянии < 100 км до Москвы"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TowageCommon"
                  ],
                  "name": "Эвакуация"
                },
                {
                  "fieldgroup": [
                    "ServiceCommon",
                    [
                      {
                        "name": "caseAddress",
                        "label": "Адрес кейса",
                        "subform": "Address"
                      },
                      {
                        "name": "distanceMoscowApproved",
                        "type": "checkbox",
                        "label": "Кейс на расстоянии < 100 км до Москвы"
                      },
                      {
                        "name": "serviceApproved",
                        "type": "checkbox",
                        "label": "Услуга может быть оказана по программе"
                      }
                    ],
                    "TechCommon"
                  ],
                  "name": "Техническая помощь"
                }
              ]
            }
          ],
          "name": "Autokraft Assistance",
          "conditions": [
            {
              "name": "VINApproved",
              "type": "checkbox",
              "label": "VIN в списке участников программы"
            },
            {
              "name": "programEndDate",
              "label": "Срок действия программы",
              "datepicker": true
            },
            {
              "name": "programNotExpired",
              "type": "checkbox",
              "label": "Программа действует"
            },
            {
              "name": "programEndMilage",
              "label": "Ограничение по пробегу"
            },
            {
              "name": "milage",
              "label": "Пробег"
            },
            {
              "name": "milageApproved",
              "type": "checkbox",
              "label": "Межсервисный интервал не пройден"
            },
            {
              "name": "isMember",
              "type": "checkbox",
              "label": "Клиент участвует в программе"
            }
          ]
        }
      ]
    }
  ],
  "commons": [
    {
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
      ],
      "name": "TaxiCommon"
    },
    {
      "fields": [
        {
          "name": "caseAddress",
          "label": "Адрес кейса",
          "subform": "Address"
        }
      ],
      "name": "otelCommon"
    },
    {
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
      ],
      "name": "TechCommon"
    },
    {
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
      ],
      "name": "TransportationCommon"
    },
    {
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
      ],
      "name": "ReplacementVehicleCommon"
    },
    {
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
      ],
      "name": "SparesCommon"
    },
    {
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
      ],
      "name": "NotificationCommon"
    },
    {
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
      ],
      "name": "ServiceCommon"
    },
    {
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
      ],
      "name": "TechSelectedCommon"
    },
    {
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
      ],
      "name": "TowageCommon"
    },
    {
      "fields": [
        {
          "name": "transportCarTo",
          "label": "Куда доставить",
          "subform": "Address"
        }
      ],
      "name": "TransportDeliveryCommon"
    },
    {
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
      ],
      "name": "SoberDriverCommon"
    },
    {
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
      ],
      "name": "TransportationSelectedCommon"
    }
  ]
}
