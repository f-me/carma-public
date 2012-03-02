function metaPages() {
  return {
    "call": {
      left: ["CallInfo"],
      right:["SelectCase","CallerDetails"],
      warnings:[]
    },
    "case": {
      left: ["CaseInfo"],
      right: ["ProgramInfo","subform","CaseHistory"],
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
    Contractor: {
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
              {"Contractor.company":0
              ,"Contractor.name":2
              ,"Contractor.phones":3}
            ,source:"/api/search_contractor"
            }}
          }
        ]
    },
    Dealer: {
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
              {"Dealer.company":0
              ,"Dealer.city":1
              ,"Dealer.program":2}
            ,source:"/api/search_dealer"}}
          }]
    },
    CaseInfo: {
      fields:
        [{wazzup:{label:"Что случилось?", type:"textarea"}}
        ,{program:{label:"Программа", data:Programs(), levels:2}}
        ,{vin:{label:"VIN",subform:"CarDetails",required:true}}
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
    CarDetails: {
      title:"Информация о машине",
      fields:
        [ {model: {label:"Модель",data: "CarModels",hotkey:"alt+z"}}
        , {transmission:
          {type:"options"
          ,label:"Коробка передач"
          ,default:0
          ,data:"Transmission"
          }}
        , {sell_date: {label:"Дата продажи",required:true,datepicker:true}}
        , {checkup_date: {label:"Дата последнего ТО",datepicker:true}}
        , {mileage_at_checkup: {label:"Пробег на последнем ТО",validate:/^[ \d]*$/}}
        , {current_mileage: {label:"Текущий пробег",validate:/^[ \d]*$/}}
        , {plate_number:
          {label:"Госномер"
          ,required:true
          }}
        ]
    },
    Address: {
      title: "Адрес",
      fields:
        [ {addr: {label:"Адрес"}}
        , {coords: {label:"Координаты"}}
        , {notes: {label:"Примечания", type:"textarea"}}
        , {map: {type:"map"}}
        ]
    },
    ProgramInfo:{fields:[]},
    CaseHistory:{fields:[]},
    RequiredFields:{fields:[]},
    EmptyForm:{fields:[]},
    subform:{fields:[]}
  };
}
