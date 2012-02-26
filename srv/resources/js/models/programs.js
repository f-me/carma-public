function Programs() {
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
              conditions: // [].concat(serviceCommonFields, xxx, towageCommonFields)
                [{caseAddress:{label:"Адрес кейса",type:"Address"}}
                ,{towDealer:{label:"Дилер",type:"Dealer"}}
                ,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
                ,{closeDealersPresent:{label:"Есть дилеры на расстоянии <125 км",type:"checkbox"}}
                ,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
                ,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ]
              }}
            ]}
          ,{"Техпомощь":
            [{"Замена колеса":{
              conditions:
                [{moreThan1Wheel:{label:"Более одного колеса",type:"checkbox"}}
                ,{caseAddress:{label:"Адрес кейса",type:"Address"}}
                ,{contractor:{label:"Подрядчик",type:"Contractor"}}
                ,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
                ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
                ]
              }}
            ]}
          ]
        }}
      ,{"Коммерческие автомобили":
        {info:"условия программы"
        ,conditions: []
        ,services: []
        }}
      ]}
    ,{"GM":
      [{"Chevrolet Korea":{}}
      ,{"Chevrolet NA":{}}
      ,{"Cadillac до 2012":{}}
      ,{"Opel (после 01.04.2011)":{}}
      ,{"Hummer":{}}
      ,{"Caddilac после 2012":{}}
      ]}
    ,{"B2B":
      [{"Arc B2B":{}}
      ,{"RTR Hyundai":{}}
      ,{"АРВАЛ":{}}
      ,{"Дженсер Ясенево":{}}
      ,{"ИП Трубкин":{}}
      ,{"ДЦ Автоимпорт":{}}
      ,{"ДЦ ТВД-Авто":{}}
      ,{"3S Телематика":{}}
      ]}
    ,{"ACTA":
      [{"Bentley":{}}
      ,{"Aston Martin":{}}
      ]}
    ,{"Другие":
      [{"Заказ билетов":{}}
      ,{"B2C":{}}
      ,{"Ford":{}}
      ,{"BP":{}}
      ,{"Рус Лан":{}}
      ,{"Атлант М":{}}
      ,{"Chartis Assistance":{}}
      ,{"Autokraft Assistance":{}}
      ]}
    ]);
}
/*
	Service: {
		fields:
			[{status:{label:"Статус услуги",data:"ServiceStatuses",required:true}}
			,{paymentType:{label:"Тип оплаты",data:"PaymentTypes",type:"options"}}
			,{cost:{label:"Стоимость"}}
			,{requiredTime:{label:"Ожидаемое время оказания услуги",datetime:true}}
			,{falseServices:{label:"Ложный вызов",data:"FalseStatuses"}}
			,{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{serviceConditions:{type:"form"}}
			]
	},
	Towage: {
		fields:
			[{towerType:{label:"Тип эвакуатора",data:"TowerTypes",required:true}}
			,{towType:{label:"Тип эвакуации",data:"TowTypes",required:true}}
			,{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}
			,{towAddress:{label:"Адрес доставки",type:"Address",data:"Dealer.address"}}
			,{towContractor:{label:"Подрядчик",type:"Contractor"}}
			,{wheelsUnblocked:{label:"Колёса не заблокированы",type:"checkbox"}}
			,{manipulatorPossible:{label:"Есть место для манипулятора",type:"checkbox"}}
			]
	},
	VWMotorWheelReplacementConditions: {
	    fields:
	},
    WheelReplacement: {
	    fields:
		    [{techType:{label:"Тип техпомощи",data:"TechTypes:Замена колеса"}}
			,{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{techContractor:{label:"Подрядчик",type:"Contractor"}}
			,{techComments:{label:"Примечания",type:"textarea"}}
			]
    },	
	VWMotorBatteryChargeConditions: {
	    fields:
		    [{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{contractor:{label:"Подрядчик",type:"Contractor"}}
			,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <125 км",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	BatteryCharge: {
	    fields:
		    [{techType:{label:"Тип техпомощи",data:"TechTypes:Зарядка АКБ"}}
			,{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{techContractor:{label:"Подрядчик",type:"Contractor"}}
			,{techComments:{label:"Примечания",type:"textarea"}}
			]
	},
	VWMotorReplacementVehicleConditions: {  //условия на легковые и грузовые одинакоы!!!
	    fields:
		    [{otherServiceNotUsed:{label:"Клиент не пользовался услугой Гостиница",type:"checkbox"}}
			,{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{SelfTow:{label:"Клиент самостоятельно добрался до дилера",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}
			,{dealerApprovedTow:{label:"Дилер подтвердил самостоятельную эвакуацию клиента",type:"checkbox"}}
			,{startRepairDate:{label:"Дата начала ремонта",date:true}}
			,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",date:true}}
			,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	ReplacementVehicle: {
	    fields:
		    [{towDealer:{label:"Дилер",type:"Dealer"}}
			,{rentAddress:{label:"Куда доставить",type:"Address"}}
			,{carClass:{label:"Класс автомобиля",data:"CarClasses"}}
			,{rentContractor:{label:"Подрядчик",type:"Contractor"}}
			]	
	},
	VWMotorHotelConditions: {  //для гостиницы условия на легковые и грузовые также совпадают
	    fields:
		    [{otherServiceNotUsed:{label:"Клиент не пользовался услугой Подменный автомобиль",type:"checkbox"}}
			,{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{SelfTow:{label:"Клиент самостоятельно добрался до дилера",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}			
			,{startRepairDate:{label:"Дата начала ремонта",date:true}}
			,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",date:true}}
			,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	Hotel: {
	    fields:
		    [{caseAddress:{label:"Адрес кейса",type:"Address"}}
			]	
	},
	VWTruckConditions: {
      fields:
        [{sellDate:{label:"Дата продажи",date:true}}
		,{sellDateCheck1:{label:"Дата продажи > 01.06.2010",type:"checkbox"}}
        ,{sellDateCheck2:{label:"Дата продажи < 2 лет",type:"checkbox"}}
        ,{make:{label:"Марка",data:"CarMarkers"}}
		,{model:{label:"Модель",data:"CarModels"}}
		,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
		,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
		,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
		]
    },
	VWTruckTowageConditions: {
		fields:
			[{closeDealersPresent:{label:"Есть дилеры на расстоянии <250 км",type:"checkbox"}}
			,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
			,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	VWTruckWheelReplacementConditions: {
	    fields:
		    [{moreThan1Wheel:{label:"Более одного колеса",type:"checkbox"}}
			,{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{contractor:{label:"Подрядчик",type:"Contractor"}}
			,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <250 км",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
    VWTruckBatteryChargeConditions: {
	    fields:
		    [{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{contractor:{label:"Подрядчик",type:"Contractor"}}
			,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <250 км",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]	
	},
	VWTruckReplacementVehicleConditions: { //условия на легковые и грузовые одинаковы!!!
	    fields:
		    [{otherServiceNotUsed:{label:"Клиент не пользовался услугой Гостиница",type:"checkbox"}}
			,{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{SelfTow:{label:"Клиент самостоятельно добрался до дилера",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}
			,{dealerApprovedTow:{label:"Дилер подтвердил самостоятельную эвакуацию клиента",type:"checkbox"}}
			,{startRepairDate:{label:"Дата начала ремонта",date:true}}
			,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",date:true}}
			,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	VWTruckHotelConditions: {  //для гостиницы условия на легковые и грузовые также совпадают
	    fields:
		    [{otherServiceNotUsed:{label:"Клиент не пользовался услугой Подменный автомобиль",type:"checkbox"}}
			,{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{SelfTow:{label:"Клиент самостоятельно добрался до дилера",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}			
			,{startRepairDate:{label:"Дата начала ремонта",date:true}}
			,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",date:true}}
			,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	GMChevroletKoreaConditions: {
        fields:
            [{sellDate:{label:"Дата продажи",date:true}}
		    ,{sellDateCheck1:{label:"Дата продажи > 15.12.2010",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 2 лет",type:"checkbox"}}
            ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
			]
    },
	GMChevroletKoreaTechnicalAssistanceConditions: {
        fields:
	        [{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	GMChevroletKoreaTowageConditions: {
		fields:
			[{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}
			,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
			,{nonAccident:{label:"Не ДТП",type:"checkbox"}}
			,{nonVandal:{label:"Не вандализм",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	GMChevroletKoreaReplacementVehicleConditions: {	
	    fields:
		    [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}
			,{startRepairDate:{label:"Дата начала ремонта",date:true}}
			,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",date:true}}	
			,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	GMChevroletKoreaHotelConditions: {	
	    fields:
		    [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{clientAddress:{label:"Место жительства",type:"Address"}}
			,{distanceApproved:{label:"Неисправность возникла на расстоянии 130 км от дома",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}			
			,{startRepairDate:{label:"Дата начала ремонта",date:true}}
			,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",date:true}}
			,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	GMChevroletKoreaTaxiConditions: {
	    fields:
		    [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	Taxi: {
	    fields:
		    [{taxiFrom:{label:"Откуда",type:"Address"}}
			,{taxiTo:{label:"Куда доставить",type:"Address"}}
			]
	},
	GMChevroletKoreaTransportationTravelConditions: {  //транспортировка (путешествие)
	    fields:
		    [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}
			,{startRepairDate:{label:"Дата начала ремонта",date:true}}
			,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",date:true}}
			,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}			
			,{attachedFiles:{label:"Приложенные файлы",type:"files"}}
			,{TravelApproved:{label:"Подтверждение путешествия предоставлено",type:"checkbox"}}					
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	Transportation: {
	     fields:
		    [{transportType:{label:"Тип транспортировки",data:"TransportTypes"}}
			,{transportFrom:{label:"Откуда",type:"Address"}}
			,{transportTo:{label:"Куда доставить",type:"Address"}}
			]
	},
	GMChevroletKoreaTransportationDeliveryConditions: {  //транспортировка (Доставка к ТС)
	    fields:
		    [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}
			,{startRepairDate:{label:"Дата начала ремонта",date:true}}
			,{plannedRepairDate:{label:"Предполагаемая дата исправления автомобиля",date:true}}
			,{longRepair:{label:"Неисправность не может быть исправлена в день обращения",type:"checkbox"}}	
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},	
	GMChevroletKoreaSparesConditions: {  //проверки доставка запчастей
	    fields:
		    [{RAMCtow:{label:"Эвакуация РАМК",type:"checkbox"}}		
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
	Spares: {   //Доставка запчастей
	    fields:
		    [{parts:{label:"Запчасти",type:"textarea"}}		
			,{partsToAddress:{label:"Куда доставить",type:"Address"}}
			]
	},
	GMChevroletNAConditions: {
	    fields:
            [{sellDate:{label:"Дата продажи",date:true}}
		    ,{sellDateCheck1:{label:"Дата продажи > 01.02.2010",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 1 года",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
		    ,{model:{label:"Модель",data:"CarModels"}}
		    ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
		    ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
		    ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
		    ]
    },
	GMCadillacBefore2012Conditions: {   //Cadillac до 2012
	    fields:
            [{sellDate:{label:"Дата продажи",date:true}}
		    ,{sellDateCheck1:{label:"Дата продажи > 01.02.2010",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 1 года",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
		    ,{model:{label:"Модель",data:"CarModels"}}
		    ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
		    ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
		    ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
		    ]
    },
	GMOpelConditions: {   //Opel после 01.04.2011
	    fields:
            [{sellDate:{label:"Дата продажи",date:true}}
		    ,{sellDateCheck1:{label:"Дата продажи > 01.04.2011",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 3 года",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
		    ,{model:{label:"Модель",data:"CarModels"}}
		    ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
		    ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
		    ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
		    ]
    },
	GMHummerConditions: {   
	    fields:
            [{sellDate:{label:"Дата продажи",date:true}}
		    ,{sellDateCheck1:{label:"Дата продажи > 01.02.2010",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 1 года",type:"checkbox"}}        
		    ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
		    ]
    },
	GMCadillac2012Conditions: {   //Cadillac после 2012
	    fields:
            [{sellDate:{label:"Дата продажи",date:true}}
		    ,{sellDateCheck1:{label:"Дата продажи > 01.01.2012",type:"checkbox"}}
            ,{sellDateCheck2:{label:"Дата продажи < 3 года",type:"checkbox"}}
            ,{make:{label:"Марка",data:"CarMarkers"}}
		    ,{model:{label:"Модель",data:"CarModels"}}
		    ,{approvedModels:{label:"Список моделей входящих в программу",type:"textarea"}}
		    ,{modelApproved:{label:"Модель входит в программу",type:"checkbox"}}
		    ,{isMember:{label:"Клиент участвует в программе",type:"checkbox"}}
		    ]
    },
	GMCadillac2012SoberDriverConditions: {   
	    fields:
            [{serviceProvided:{label:"Сколько раз предоставлена услуга"}}
		    ,{serviceRestriction:{label:"Не более трёх раз за срок действия программы"}}
            ,{restrictionApproved:{label:"Лимит не исчерпан",type:"checkbox"}}
            ,{fromAddress:{label:"Откуда",type:"Address"}}
		    ,{toAddress:{label:"Куда доставить",type:"Address"}}
		    ,{distanceFromToApproved:{label:"Расстояние <130 км",type:"checkbox"}}
		    ,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
		    ]
    },
	SoberDriver: {
	    fields:
            [{fromAddress:{label:"Откуда",type:"Address"}}
		    ,{toAddress:{label:"Куда доставить",type:"Address"}}
		    ,{multidrive:{label:"Каско МУЛЬТИДРАЙВ",type:"checkbox"}}
		    ]
    },
	GMCadillac2012FuelDeliveryConditions: {   //техпомощь - доставка топлива
	    fields:
            [{caseAddress:{label:"Адрес кейса",type:"Address"}}
		    ,{contractor:{label:"Подрядчик",type:"Contractor"}}
			,{closeContractorsPresent:{label:"Есть мастерские на расстоянии <130 км",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]	
	},
	GMCadillac2012TowageConditions: {
		fields:
			[{caseAddress:{label:"Адрес кейса",type:"Address"}}
			,{closeDealersPresent:{label:"Есть дилеры на расстоянии <130 км",type:"checkbox"}}
			,{towDealer:{label:"Дилер",type:"Dealer"}}
			,{DCapproved:{label:"Клиент договорился с ДЦ",type:"checkbox"}}
			,{serviceApproved:{label:"Услуга может быть оказана по программе",type:"checkbox"}}
			]
	},
    ProgramInfo:{fields:[]},
    CaseHistory:{fields:[]},
    RequiredFields:{fields:[]},
    EmptyForm:{fields:[]},
    Services: {
      fields:
        [{container:{type:"form"}}
        ,{choose:{label:"Вид услуги", data:"Services"}}
        ,{add:{label:"Добавить",type:"link"}}
        ]
    }
  };
}
*/
