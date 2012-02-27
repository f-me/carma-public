function Programs() {
  var ServiceCommon = {
    fields:
      [{status:{label:"Статус услуги",data:"ServiceStatuses",required:true}}
      ,{paymentType:{label:"Тип оплаты",data:"PaymentTypes",type:"options",default:0}}
      ,{cost:{label:"Стоимость"}}
      ,{requiredTime:{label:"Ожидаемое время оказания услуги",datetime:true}}
      ,{falseServices:{label:"Ложный вызов",data:"FalseStatuses"}}
      ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
      ]
  };
  var TowageCommon = {   //эвакуация
    fields:
      [{towerType:{label:"Тип эвакуатора",data:"TowerTypes",required:true}}
      ,{towType:{label:"Тип эвакуации",data:"TowTypes",required:true}}
      ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
      ,{towDealer:{label:"Дилер",subform:"Dealer"}}
      ,{towAddress:{label:"Адрес доставки",subform:"Address"}}
      ,{towContractor:{label:"Подрядчик",subform:"Contractor"}}
      ,{wheelsUnblocked:{label:"Колёса не заблокированы",type:"checkbox"}}
      ,{manipulatorPossible:{label:"Есть место для манипулятора",type:"checkbox"}}
      ]
  };
  var TechCommon = {  //тех помощь 
    fields:
       [{techType:{label:"Тип техпомощи",data:"TechTypes"}}
       ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
       ,{techContractor:{label:"Подрядчик",subform:"Contractor"}}
       ,{techComments:{label:"Примечания",type:"textarea"}}
       ]
    };
  var TechSelectedCommon = {  //тех помощь, когда указан тип
    fields:
        [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
        ,{techContractor:{label:"Подрядчик",subform:"Contractor"}}
        ,{techComments:{label:"Примечания",type:"textarea"}}
        ]
    };
  var ReplacementVehicleCommon = {  //подменный автомобиль
    fields:
       [{towDealer:{label:"Дилер",subform:"Dealer"}}
        ,{rentAddress:{label:"Куда доставить",subform:"Address"}}
        ,{carClass:{label:"Класс автомобиля",data:"CarClasses"}}
        ,{rentContractor:{label:"Подрядчик",subform:"Contractor"}}
        ]
    };
  var TransportationCommon = {  //транспортировка
    fields:
       [{transportType:{label:"Тип транспортировки",data:"TransportTypes"}}
        ,{transportFrom:{label:"Откуда",subform:"Address"}}
        ,{transportTo:{label:"Куда доставить",subform:"Address"}}
        ]
    };
  var TransportationSelectedCommon = {  //транспортировка, когда указан тип
    fields:
       [{transportFrom:{label:"Откуда",subform:"Address"}}
        ,{transportTo:{label:"Куда доставить",subform:"Address"}}
        ]
    };
  var SparesCommon = {  //доставка запчастей
    fields:
       [{parts:{label:"Запчасти",type:"textarea"}}      
        ,{partsToAddress:{label:"Куда доставить",subform:"Address"}}
        ]
    };
  var SoberDriverCommon = {  //трезвый водитель
    fields:
       [{fromAddress:{label:"Откуда",subform:"Address"}}
        ,{toAddress:{label:"Куда доставить",subform:"Address"}}
        ,{multidrive:{label:"Каско МУЛЬТИДРАЙВ",type:"checkbox"}}
        ]
    };
  var NotificationCommon = {  //информирование о происшествии
    fields:
       [{infoContact1:{label:"Контакт 1"}}      
        ,{infoPhone1:{label:"Телефон 1"}}
        ,{info1:{label:"Что сказать 1",type:"textarea"}}
        ,{infoContact2:{label:"Контакт 2"}}     
        ,{infoPhone2:{label:"Телефон 2"}}
        ,{info2:{label:"Что сказать 2",type:"textarea"}}
        ,{infoContact3:{label:"Контакт 3"}}     
        ,{infoPhone3:{label:"Телефон 3"}}
        ,{info3:{label:"Что сказать 3",type:"textarea"}}            
        ]
    };  
  var HotelCommon = {  //отель
    fields:
       [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
       ]
    };
  var TaxiCommon = {  //такси
    fields:
      [{taxiFrom:{label:"Откуда",subform:"Address"}}
      ,{taxiTo:{label:"Куда доставить",subform:"Address"}}
      ]
  };
  var TransportDeliveryCommon = {  //Доставка ТС
    fields:
      [{transportCarTo:{label:"Куда доставить",subform:"Address"}}
      ]
  };
  
  return (
    [{"VW":
      [{"Легковые автомобили":
        {info:"Описание программы в виде текста"
        ,conditions: 
          [{sellDate:{label:"Дата продажи",dateSelector:true}}
          ,{sellDateCheck1:{label:"Дата продажи > 15.01.2010",type:"checkbox"}}
          ,{sellDateCheck2:{label:"Дата продажи < 2 лет",type:"checkbox"}}
          ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
          ]
        ,services:
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <125 км",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ]}
          ,{"Техпомощь":
            [{"Замена колеса":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{moreThan1Wheel:{label:"Более одного колеса",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)
              }}
            ]}
          ,{"Техпомощь":
            [{"Зарядка АКБ":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
            ],
                TechSelectedCommon.fields)
              }}
            ]}
          ,{"":
            [{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
               [{otherServiceNotUsed:{label:"Клиент не пользовался услугой Гостиница",type:"checkbox"}}
                 ,{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                 ,{SelfTow:{label:"Клиент самостоятельно добрался до дилера",type:"checkbox"}}
                 ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                 ,{dealerApprovedTow:{label:"Дилер подтвердил самостоятельную эвакуацию клиента",type:"checkbox"}}
                 ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                 ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                 ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                 ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
            ],
               ReplacementVehicleCommon.fields)
              }}
            ]}
          ,{"":
            [{"Гостиница":{
              conditions: [].concat(
                ServiceCommon.fields,
               [{otherServiceNotUsed:{label:"Клиент не пользовался услугой Подменный автомобиль",type:"checkbox"}}
               ,{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
               ,{SelfTow:{label:"Клиент самостоятельно добрался до дилера",type:"checkbox"}}
               ,{towDealer:{label:"Дилер",subform:"Dealer"}}           
               ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
               ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
               ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
               ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
            ],
               HotelCommon.fields)
              }}
            ]}
          ]
        }}
      ,{"Коммерческие автомобили":
        {info:"условия программы"
        ,conditions: 
            [{sellDate:{label:"Дата продажи",datepicker:true}}
            ,{sellDateCheck1:{label:"Дата продажи > 01.06.2010",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 2 лет",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
            ,{model:{label:"Модель",data:"CarModels"}}
            ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
            ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
            ]
        ,services: 
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{closeDealersPresent:{label:"Есть дилеры на расстоянии <250 км",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ]}
          ,{"Техпомощь":
            [{"Замена колеса":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{moreThan1Wheel:{label:"Более одного колеса",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <250 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)
              }}
            ]}
          ,{"Техпомощь":
            [{"Зарядка АКБ":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
            ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
            ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <250 км",type:"checkbox"}}
            ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
            ],
                TechSelectedCommon.fields)
              }}
            ]}
          ,{"":
            [{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
               [{otherServiceNotUsed:{label:"Клиент не пользовался услугой Гостиница",type:"checkbox"}}
                 ,{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                 ,{SelfTow:{label:"Клиент самостоятельно добрался до дилера",type:"checkbox"}}
                 ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                 ,{dealerApprovedTow:{label:"Дилер подтвердил самостоятельную эвакуацию клиента",type:"checkbox"}}
                 ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                 ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                 ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                 ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
            ],
               ReplacementVehicleCommon.fields)
              }}
            ]}
          ,{"":
            [{"Гостиница":{
              conditions: [].concat(
                ServiceCommon.fields,
               [{otherServiceNotUsed:{label:"Клиент не пользовался услугой Подменный автомобиль",type:"checkbox"}}
               ,{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
               ,{SelfTow:{label:"Клиент самостоятельно добрался до дилера",type:"checkbox"}}
               ,{towDealer:{label:"Дилер",subform:"Dealer"}}           
               ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
               ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
               ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
               ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
               ],
               HotelCommon.fields)
              }}
            ]}
          ]
        }}
      ]}
    ,{"GM":
      [{"Chevrolet Korea":
        {conditions:
          [{sellDate:{label:"Дата продажи",datepicker:true}}
          ,{sellDateCheck1:{label:"Дата продажи > 15.12.2010",type:"checkbox"}}
          ,{sellDateCheck2:{label:"Дата продажи < 2 лет",type:"checkbox"}}
          ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
          ]
        ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
           ,{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}} 
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                ReplacementVehicleCommon.fields)
              }}
            ,{"Гостиница":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{clientAddress:{label:"Место жительства",subform:"Address"}}
                ,{distanceApproved:{label:"Неисправность возникла на расстоянии 130 км от дома",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}          
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                HotelCommon.fields)
              }}
            ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
            ,{"Доставка запчастей":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{parts:{label:"Запчасти",type:"textarea"}}     
                ,{partsToAddress:{label:"Куда доставить",subform:"Address"}}
                ],
                SparesCommon.fields)
              }}            
            ]}
          ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}            
                ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
                ,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}                    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }},
              {"Доставка к ТС":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }}
            ]}
          ]
        }}     
      ,{"Chevrolet NA":
       {conditions:
          [{sellDate:{label:"Дата продажи",datepicker:true}}
          ,{sellDateCheck1:{label:"Дата продажи > 15.12.2010",type:"checkbox"}}
          ,{sellDateCheck2:{label:"Дата продажи < 2 лет",type:"checkbox"}}
          ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
          ]
        ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
           ,{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}} 
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                ReplacementVehicleCommon.fields)
              }}
            ,{"Гостиница":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{clientAddress:{label:"Место жительства",subform:"Address"}}
                ,{distanceApproved:{label:"Неисправность возникла на расстоянии 130 км от дома",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}          
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                HotelCommon.fields)
              }}
            ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
            ,{"Доставка запчастей":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{parts:{label:"Запчасти",type:"textarea"}}     
                ,{partsToAddress:{label:"Куда доставить",subform:"Address"}}
                ],
                SparesCommon.fields)
              }}            
            ]}
          ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}            
                ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
                ,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}                    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }},
              {"Доставка к ТС":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }}
            ]}
          ]
        }}     
      ,{"Cadillac до 2012":
        {conditions:
            [{sellDate:{label:"Дата продажи",datepicker:true}}
            ,{sellDateCheck1:{label:"Дата продажи > 01.02.2010",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 1 года",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
            ,{model:{label:"Модель",data:"CarModels"}}
            ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
            ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
            ]
        ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
           ,{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}} 
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                ReplacementVehicleCommon.fields)
              }}
            ,{"Гостиница":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{clientAddress:{label:"Место жительства",subform:"Address"}}
                ,{distanceApproved:{label:"Неисправность возникла на расстоянии 130 км от дома",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}          
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                HotelCommon.fields)
              }}
            ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
            ,{"Доставка запчастей":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{parts:{label:"Запчасти",type:"textarea"}}     
                ,{partsToAddress:{label:"Куда доставить",subform:"Address"}}
                ],
                SparesCommon.fields)
              }}            
            ]}
          ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}            
                ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
                ,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}                    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }},
              {"Доставка к ТС":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }}
            ]}
          ]
        }}
      ,{"Opel (после 01.04.2011)":
        {conditions:
            [{sellDate:{label:"Дата продажи",datepicker:true}}
            ,{sellDateCheck1:{label:"Дата продажи > 01.02.2010",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 1 года",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
            ,{model:{label:"Модель",data:"CarModels"}}
            ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
            ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
            ]
        ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
           ,{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}} 
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                ReplacementVehicleCommon.fields)
              }}
            ,{"Гостиница":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{clientAddress:{label:"Место жительства",subform:"Address"}}
                ,{distanceApproved:{label:"Неисправность возникла на расстоянии 130 км от дома",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}          
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                HotelCommon.fields)
              }}
            ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
            ,{"Доставка запчастей":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{parts:{label:"Запчасти",type:"textarea"}}     
                ,{partsToAddress:{label:"Куда доставить",subform:"Address"}}
                ],
                SparesCommon.fields)
              }}            
            ]}
          ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}            
                ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
                ,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}                    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }},
              {"Доставка к ТС":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }}
            ]}
          ]
        }}
      ,{"Hummer":
        {conditions:
            [{sellDate:{label:"Дата продажи",datepicker:true}}
            ,{sellDateCheck1:{label:"Дата продажи > 01.04.2011",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 3 года",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
            ,{model:{label:"Модель",data:"CarModels"}}
            ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
            ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
            ]
        ,services:
           [{"":
            [{"Техническая помошь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
           ,{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}} 
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                ReplacementVehicleCommon.fields)
              }}
            ,{"Гостиница":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{clientAddress:{label:"Место жительства",subform:"Address"}}
                ,{distanceApproved:{label:"Неисправность возникла на расстоянии 130 км от дома",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}          
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                HotelCommon.fields)
              }}
            ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
            ,{"Доставка запчастей":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{parts:{label:"Запчасти",type:"textarea"}}     
                ,{partsToAddress:{label:"Куда доставить",subform:"Address"}}
                ],
                SparesCommon.fields)
              }}            
            ]}
          ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}            
                ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
                ,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}                    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }},
              {"Доставка к ТС":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }}
            ]}
          ]
        }}
      ,{"Caddilac после 2012":
        {conditions:
            [{sellDate:{label:"Дата продажи",datepicker:true}}
            ,{sellDateCheck1:{label:"Дата продажи > 01.01.2012",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 3 года",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
            ,{model:{label:"Модель",data:"CarModels"}}
            ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
            ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
            ]
        ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
           ,{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}} 
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                ReplacementVehicleCommon.fields)
              }}
            ,{"Гостиница":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{clientAddress:{label:"Место жительства",subform:"Address"}}
                ,{distanceApproved:{label:"Неисправность возникла на расстоянии 130 км от дома",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}          
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                HotelCommon.fields)
              }}
            ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
            ,{"Доставка запчастей":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{parts:{label:"Запчасти",type:"textarea"}}     
                ,{partsToAddress:{label:"Куда доставить",subform:"Address"}}
                ],
                SparesCommon.fields)
              }}
            ,{"Трезвый водитель":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{serviceProvided:{label:"Сколько раз предоставлена услуга"}}
                ,{serviceRestriction:{label:"Не более трёх раз за срок действия программы"}}
                ,{restrictionApproved:{label:"Лимит не исчерпан",type:"checkbox"}}
                ,{fromAddress:{label:"Откуда",subform:"Address"}}
                ,{toAddress:{label:"Куда доставить",subform:"Address"}}
                ,{distanceFromToApproved:{label:"Расстояние <130 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                SoberDriverCommon.fields)
              }}                  
            ]}
          ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}            
                ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
                ,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}                    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }},
              {"Доставка к ТС":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}    
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TransportationSelectedCommon.fields)
              }}
            ]}
          ,{"Техпомощь":
            [{"Доставка топлива":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <130 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)
              }}
            ]}
          ]
        }}
      ]}
    ,{"B2B":
      [{"Arc B2B":
      {conditions:
       [{companyApproved:{label:"Обращение компании Arc",type:"checkbox"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
         ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
              ,{"Гостиница":{
              conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
              ,{"Транспортировка":{
              conditions: [].concat(ServiceCommon.fields, TransportationCommon.fields)
              }}
              ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}
              ,{"Доставка запчастей":{
               conditions: [].concat(ServiceCommon.fields, SparesCommon.fields)
              }}
               ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, NotificationCommon.fields)
              }}
              ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, SoberDriverCommon.fields)
              }}
             ]}
            ]}
           }
  ,{"RTR Hyundai":
      {conditions:
       [{companyApproved:{label:"Обращение компании RTR Hyundai",type:"checkbox"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
         ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
              ,{"Гостиница":{
              conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
              ,{"Транспортировка":{
              conditions: [].concat(ServiceCommon.fields, TransportationCommon.fields)
              }}
              ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}
              ,{"Доставка запчастей":{
               conditions: [].concat(ServiceCommon.fields, SparesCommon.fields)
              }}
               ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, NotificationCommon.fields)
              }}
              ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields,SoberDriverCommon.fields)
              }}
             ]}
            ]}
           }
  ,{"Дженсер Ясенево":
      {conditions:
       [{companyApproved:{label:"Обращение компании Дженсер Ясенево",type:"checkbox"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
         ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
              ,{"Гостиница":{
              conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
              ,{"Транспортировка":{
              conditions: [].concat(ServiceCommon.fields, TransportationCommon.fields)
              }}
              ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}
              ,{"Доставка запчастей":{
               conditions: [].concat(ServiceCommon.fields, SparesCommon.fields)
              }}
               ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, NotificationCommon.fields)
              }}
              ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, SoberDriverCommon.fields)
              }}
             ]}
            ]}
           }           
  ,{"ИП Трубкин":
      {conditions:
       [{companyApproved:{label:"Обращение компании ИП Трубкин",type:"checkbox"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
         ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
              ,{"Гостиница":{
              conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
              ,{"Транспортировка":{
              conditions: [].concat(ServiceCommon.fields, TransportationCommon.fields)
              }}
              ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}
              ,{"Доставка запчастей":{
               conditions: [].concat(ServiceCommon.fields, SparesCommon.fields)
              }}
               ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, NotificationCommon.fields)
              }}
              ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields,SoberDriverCommon.fields)
              }}
             ]}
            ]}
           }
  ,{"ДЦ Автоимпорт":
      {conditions:
       [{companyApproved:{label:"Обращение компании ДЦ Автоимпорт",type:"checkbox"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
         ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
              ,{"Гостиница":{
              conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
              ,{"Транспортировка":{
              conditions: [].concat(ServiceCommon.fields, TransportationCommon.fields)
              }}
              ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}
              ,{"Доставка запчастей":{
               conditions: [].concat(ServiceCommon.fields, SparesCommon.fields)
              }}
               ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, NotificationCommon.fields)
              }}
              ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields,SoberDriverCommon.fields)
              }}
             ]}
            ]}
           }
  ,{"ДЦ ТВД-Авто":
      {conditions:
       [{companyApproved:{label:"Обращение компании ДЦ ТВД-Авто",type:"checkbox"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
         ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
              ,{"Гостиница":{
              conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
              ,{"Транспортировка":{
              conditions: [].concat(ServiceCommon.fields, TransportationCommon.fields)
              }}
              ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}
              ,{"Доставка запчастей":{
               conditions: [].concat(ServiceCommon.fields, SparesCommon.fields)
              }}
               ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, NotificationCommon.fields)
              }}
              ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, SoberDriverCommon.fields)
              }}
             ]}
            ]}
           }
 ,{"АРВАЛ":
      {conditions:
       [{companyApproved:{label:"Обращение компании АРВАЛ",type:"checkbox"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
         ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
             ,{"Гостиница":{
              conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
            ]}
           ]}
           }
 ,{"3S Телематика":
      {conditions:
       [{companyApproved:{label:"Обращение компании 3S Телематика",type:"checkbox"}}
        ,{companyEmployees:{label:"Контактные лица",subform:"Employees"}}
        ,{employeeApproved:{label:"Сотрудник авторизован",type:"checkbox"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
            ]
         ,services:
          [{"":
            [{"Техническая помошь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
              ,{"Гостиница":{
              conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}
              ,{"Транспортировка":{
              conditions: [].concat(ServiceCommon.fields, TransportationCommon.fields)
              }}
              ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}
              ,{"Доставка запчастей":{
               conditions: [].concat(ServiceCommon.fields, SparesCommon.fields)
              }}
               ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, NotificationCommon.fields)
              }}
              ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, SoberDriverCommon.fields)
              }}
            ]}
          ]
        }}
      ]}
    ,{"ACTA":
      [{"Bentley":{conditions:
          [{companyApproved:{label:"Обращение компании ACTA",type:"checkbox"}}
            ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
            ,{GOPApproved:{label:"GOP предоставлена",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
        ,services:
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
            ,{"Гостиница":{
               conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
            ,{"Такси":{
              conditions: [].concat(ServiceCommon.fields, TaxiCommon.fields)
              }}
            ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}            
            ]}
          ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(ServiceCommon.fields, TransportationSelectedCommon.fields)
              }}
            ]}
          ]
        }}
      ,{"Aston Martin":{conditions:
          [{companyApproved:{label:"Обращение компании ACTA",type:"checkbox"}}
            ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
            ,{GOPApproved:{label:"GOP предоставлена",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
        ]
        ,services:
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}
            ,{"Техпомощь":{
              conditions: [].concat(ServiceCommon.fields, TechCommon.fields)
              }}              
            ,{"Подменный автомобиль":{
              conditions: [].concat(ServiceCommon.fields, ReplacementVehicleCommon.fields)
              }}
            ,{"Гостиница":{
               conditions: [].concat(ServiceCommon.fields, HotelCommon.fields)
              }}
            ,{"Такси":{
              conditions: [].concat(ServiceCommon.fields, TaxiCommon.fields)
              }}
            ,{"Доставка ТС":{
              conditions: [].concat(ServiceCommon.fields, TransportDeliveryCommon.fields)
              }}            
            ]}
          ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(ServiceCommon.fields, TransportationSelectedCommon.fields)
              }}
            ]}
          ]
        }}
      ]}
    ,{"Другие":
      [{"Заказ билетов":{}}
      ,{"B2C":{conditions:
            [{memberNumber:{label:"Карта участника"}}
            ,{memberApproved:{label:"Участник программы",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
            ]
        ,services:
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{serviceProvided:{label:"Сколько раз услуга Эвакуация предоставлена"}}
                ,{serviceRestriction:{label:"Ограничение по количеству раз предоставления услуги"}}
                ,{restrictionApproved:{label:"Лимит не исчерпан",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceApproved:{label:"Кейс на расстоянии < 50 км до Москвы или < 30 км до города",type:"checkbox"}}           
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ]}
          ,{"Техпомощь":
              [{"Подвоз топлива":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{serviceProvided:{label:"Сколько раз услуга Техпомощь - Подвоз топлива предоставлена"}}
                ,{serviceRestriction:{label:"Ограничение по количеству раз предоставления услуги"}}
                ,{restrictionApproved:{label:"Лимит не исчерпан",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceApproved:{label:"Кейс на расстоянии < 50 км до Москвы или < 30 км до города",type:"checkbox"}}           
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)
              }},
              {"Запуск двигателя":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{serviceProvided:{label:"Сколько раз услуга Техпомощь - Запуск двигателя предоставлена"}}
                ,{serviceRestriction:{label:"Ограничение по количеству раз предоставления услуги"}}
                ,{restrictionApproved:{label:"Лимит не исчерпан",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceApproved:{label:"Кейс на расстоянии < 50 км до Москвы или < 30 км до города",type:"checkbox"}}           
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)}}
              ,{"Замена колеса":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{serviceProvided:{label:"Сколько раз услуга Техпомощь - Замена колеса предоставлена"}}
                ,{serviceRestriction:{label:"Ограничение по количеству раз предоставления услуги"}}
                ,{restrictionApproved:{label:"Лимит не исчерпан",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceApproved:{label:"Кейс на расстоянии < 50 км до Москвы или < 30 км до города",type:"checkbox"}}           
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)}
              },
              {"Вскрытие автомобиля":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{serviceProvided:{label:"Сколько раз услуга Техпомощь - Вскрытие автомобиля предоставлена"}}
                ,{serviceRestriction:{label:"Ограничение по количеству раз предоставления услуги"}}
                ,{restrictionApproved:{label:"Лимит не исчерпан",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceApproved:{label:"Кейс на расстоянии < 50 км до Москвы или < 30 км до города",type:"checkbox"}}           
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)}
              }
            ]}          
          ]
        }}
      ,{"Ford":{conditions:
            [{VINApproved:{label:"VIN в списке участников программы",type:"checkbox"}}
            ,{lastTODate:{label:"Дата последнего ТО",datepicker:true}}
            ,{TOApproved:{label:"Дата последнего ТО < 1 года",type:"checkbox"}}
            ,{milageTO:{label:"Пробег на последнем ТО"}}
            ,{milage:{label:"Пробег"}}
            ,{milageApproved:{label:"Межсервисный интервал не пройден",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
            ]
        ,services:
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <200 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
              ,{"Техническая помощь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <200 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
              ,{"Информирование о происшествии":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}        
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                NotificationCommon.fields)
              }}
              ,{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}   
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                ReplacementVehicleCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}              
            ]}
            ]}}
      ,{"BP":{conditions:
            [{companyApproved:{label:"Обращение компании BP",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
            ]
        ,services:
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(ServiceCommon.fields, TowageCommon.fields)
              }}              
            ]},
          {"Техпомощь":
              [{"Слив топлива":{
               conditions: [].concat(ServiceCommon.fields, TechSelectedCommon.fields)
              }}
            ]}
                    
            
            
            
            
            
            ]}}
      ,{"Рус Лан":{conditions:
          [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
            ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
            ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
            ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
            ]
         ,services:
          [{"":
           [{"Техпомощь":
            [{"Замена колеса":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)
              }}
            ]}
          ,{"Техпомощь":
            [{"Зарядка АКБ":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)
              }}
            ]}
            ,{"Техпомощь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                 [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии < 125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ]}
          ]
        }}
      ,{"Атлант М":{conditions:
          [{VINApproved:{label:"VIN в списке участников программы",type:"checkbox"}}
            ,{programEndDate:{label:"Срок действия программы",datepicker:true}}
            ,{programNotExpired:{label:"Программа действует",type:"checkbox"}}
            ,{programEndMilage:{label:"Ограничение по пробегу"}}
            ,{milage:{label:"Пробег"}}
            ,{milageApproved:{label:"Межсервисный интервал не пройден",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
            ]
         ,services:
          [{"":
            [{"Техпомощь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}
            ,{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                 [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{contractor:{label:"Подрядчик",subform:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии < 125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
            ,{"Информирование о происшествии":{
               conditions: [].concat(ServiceCommon.fields, NotificationCommon.fields)
              }}
            ]}
            ]
      }}
      ,{"Chartis Assistance":{conditions:
            [{VINApproved:{label:"VIN в списке участников программы",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
            ]
        ,services:
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceMskSpbApproved:{label:"Кейс на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
              ,{"Подменный автомобиль":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}   
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                ReplacementVehicleCommon.fields)
              }}
              ,{"Такси":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TaxiCommon.fields)
              }}              
            ]}
            ,{"Техпомощь":
              [{"Зарядка АКБ":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceMskSpbApproved:{label:"Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)
              }},
              {"Замена колеса":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceMskSpbApproved:{label:"Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)}}
              , {"Вскрытие автомобиля":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceMskSpbApproved:{label:"Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)}
              },
              {"Подвоз топлива":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceMskSpbApproved:{label:"Кейса на расстоянии < 100 км до Москвы или < 100 км до Санкт-Петербурга",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechSelectedCommon.fields)}
              }]},
            ,{"Транспортировка":
              [{"Путешествие":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{clientAddress:{label:"Место жительства",subform:"Address"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{dealerClientDistanceApproved:{label:"Дилер в 100 км от местра проживания клиента",type:"checkbox"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
                ,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}            
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],              
                TransportationSelectedCommon.fields)
              }},
              {"Доставка к ТС":{
               conditions: [].concat(
                ServiceCommon.fields,
                [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{clientAddress:{label:"Место жительства",subform:"Address"}}
                ,{towDealer:{label:"Дилер",subform:"Dealer"}}
                ,{dealerClientDistanceApproved:{label:"Дилер в 100 км от местра проживания клиента",type:"checkbox"}}
                ,{startRepairDate:{label:"Дата начала ремонта",datepicker:true}}
                ,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",datepicker:true}}
                ,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
                ,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
                ,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}            
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],              
                TransportationSelectedCommon.fields)
              }},
            ]}
                      
              
              
              ]}}
      ,{"Autokraft Assistance":{conditions:
             [{VINApproved:{label:"VIN в списке участников программы",type:"checkbox"}}
            ,{programEndDate:{label:"Срок действия программы",datepicker:true}}
            ,{programNotExpired:{label:"Программа действует",type:"checkbox"}}
            ,{programEndMilage:{label:"Ограничение по пробегу"}}
            ,{milage:{label:"Пробег"}}
            ,{milageApproved:{label:"Межсервисный интервал не пройден",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}} 
            ]
        ,services:
          [{"":
            [{"Эвакуация":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceMoscowApproved:{label:"Кейс на расстоянии < 100 км до Москвы",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TowageCommon.fields)
              }}
              ,{"Техническая помощь":{
              conditions: [].concat(
                ServiceCommon.fields,
                [{caseAddress:{label:"Адрес кейса",subform:"Address"}}
                ,{distanceMoscowApproved:{label:"Кейс на расстоянии < 100 км до Москвы",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ],
                TechCommon.fields)
              }}          
            ]}
            ]}}
      ]}
  ]
);
}
