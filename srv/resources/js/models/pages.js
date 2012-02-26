function metaPages() {
  return {
    "call": {
      left: ["CallInfo"],
      right:["SelectCase","CallerDetails"],
      warnings:[]
    },
    "case": {
      left: ["CaseInfo"],
      right: ["ProgramInfo","CaseHistory"],
      warnings: ["RequiredFields"]
    },
  };
}

function metaForms() {
  return {
    CallInfo: {
      fields:
        [{wazzup:{label:"Что случилось", type:"textarea"}}
        ,{callType:{label:"Тип звонка", data:"CallType"}}
        ,{callerType:
          {label:"Кто звонит?"
          ,type:"options"
          ,data:"CallerType"
          ,default:0}}
        ]
    },
    CallerDetails: {
      fields:[{caller:{type:"form",exposeId:true}}],
      dependencies:
        {caller: {
          dependsOnValue:[
            {field:"CallInfo.callerType"
            ,value:
              ["GeneralContact"
              ,"SelectContractor"
              ,"SelectDealer"
              ,"GeneralContact"
              ,"GeneralContact"]}]
        }}
    },
    GeneralContact: {
      fields:
        [{name:{label:"ФИО"}}
        ,{phone:{label:"Телефоны",validate:/^[+ ,;\d]*$/}}]
    },
    SelectCase: {
      fields:
        [{query:{label:"Поиск в кейсах"}}
        ,{cases:{
          type:"searchTable",
          searchTable:
            {columns:
              ["#",
              ,"Дата",
              ,"Программа",
              ,"Услуга",
              ,"Госномер",
              ,"Статус",
              ,"ФИО",
              ,"Телефон"]
            ,query: {"SelectCase.query":"*"}
            ,source:"/api/search_case"
            }}
          }
        ,{"new":{label:"Новый кейс",type:"link"}}
        ]
    },
    SelectContractor: {
      fields:
        [{company:{label:"Компания"}}
        ,{name:{label:"ФИО"}}
        ,{phones:{label:"Контактные телефоны"}}
        ,{save:{label:"Сохранить",type:"link"}}
        ,{contractor:{
          type:"searchTable",
          searchTable:
            {columns:
              ["Компания"
              ,"Город"
              ,"ФИО"
              ,"Телефон"
              ,"Услуга"]
            ,query:
              {"SelectContractor.company":0
              ,"SelectContractor.name":2
              ,"SelectContractor.phones":3}
            ,source:"/api/search_contractor"
            }}
          }
        ]
    },
    SelectDealer: {
      fields:
        [{company:{label:"Компания"}}
        ,{city:{label:"Город"}}
        ,{program:{label:"Программа"}}
        ,{save:{label:"Сохранить",type:"link"}}
        ,{dealer:{
          type:"searchTable",
          searchTable:
            {columns:
              ["Компания"
              ,"Город"
              ,"Программа"
              ,"Адрес сервисного отдела"
              ,"Телефон"]
            ,query:
              {"SelectDealer.company":0
              ,"SelectDealer.city":1
              ,"SelectDealer.program":2}
            ,source:"/api/search_dealer"}}
          }]
    },
    CaseInfo: {
      fields:
        [{wazzup:{label:"Что случилось?", type:"textarea"}}
        ,{program:{label:"Программа", data:Programs(), levels:2}}
        ,{vin:{label:"VIN",required:true}}
        ,{car:{type:"staticText",ephemeral:true}}
        ,{programConditions:{type:"form",dependsOn:"CaseInfo.program:conditions"}}
        ,{plate:{label:"Госномер",required:true}}
        ,{contactName:{label:"Контактное лицо",required:true}}
        ,{contactPhone:{label:"Контактный телефон",required:true}}
        ,{diagnosis1:{label:"Диагностика",data:"Diagnosis1"}}
        ,{diagnodis2:{data:"Diagnosis2"}}
        ,{diagnodis3:{data:"Diagnosis3"}}
        ,{diagnodis4:{data:"Diagnosis4"}}
        ,{services:{type:"container"}}
        ,{chooseService:{label:"Вид услуги", dependsOn:"CaseInfo.program:services"}}
        ,{addService:{label:"Добавить",type:"link"}}
        ]
    },
    
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
    ProgramInfo:{fields:[]},
    CaseHistory:{fields:[]},
    RequiredFields:{fields:[]},
    EmptyForm:{fields:[]}
  };
}
