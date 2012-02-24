function metaPages() {
  return {
    "call": {
      left: ["CallInfo"],
      right:["SelectCase","CallerDetails"],
      warnings:[]
    },
    "case": {
      left: ["CaseInfo", "Services", "AddService"],
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
        ,{callType:{label:"Тип звонка", data:"callTypes"}}
        ,{callerType:
          {label:"Кто звонит?"
          ,type:"options"
          ,data:
            ["Клиент"
            ,"Подрядчик"
            ,"Дилерский центр"
            ,"Заказчик программы"
            ,"Другое"]
          ,default:0}}
        ]
    },
    CallerDetails: {
      fields:[{caller:{type:"form",exposeId:true}}],
      dependencies:
        {caller: {
          dependsOn:"CallInfo.callerType",
          value:
            ["GeneralContact"
            ,"SelectContractor"
            ,"SelectDealer"
            ,"GeneralContact"
            ,"GeneralContact"]
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
        ,{program:{label:"Программа", data:"Programs", required:true}}
        ,{vin:{label:"VIN",required:true}}
        ,{car:{type:"staticText",ephemeral:true}}
        ,{programConditions:{type:"form",dataInline:true}}
        ,{plate:{label:"Госномер",required:true}}
        ,{contactName:{label:"Контактное лицо",required:true}}
        ,{contactPhone:{label:"Контактный телефон",required:true}}
        ,{diagnosis1:{label:"Диагностика",data:"Diagnosis1"}}
        ,{diagnodis2:{data:"Diagnosis2"}}
        ,{diagnodis3:{data:"Diagnosis3"}}
        ,{diagnodis4:{data:"Diagnosis4"}}
        ],
      dependencies: {
        programConditions: {
          dependsOn:"CaseInfo.program",
          default:"EmptyForm",
          value:["VWMotorConditions","VWTruckConditions"]
        }}
    },
    VWMotorConditions: {
      fields:
        [{sellDateCheck1:{label:"Дата продажи > 15.01.2010",type:"check"}}
        ,{sellDateCheck2:{label:"Дата продажи < 2 лет",type:"check"}}
        ,{isMember:{label:"Клиент участвует в программе",type:"check"}}]
    }
  };
}

