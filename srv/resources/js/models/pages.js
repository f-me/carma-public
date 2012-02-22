function metaPages() {
  return {
    "call": {
      left: ["callInfo"],
      right:["selectCase"]
    },
    "case": {
      left: ["caseInfo", "services", "addService"],
      right: ["programInfo","caseHistory"],
      warnings: ["requiredFields"]
    },
  };
}

function metaForms() {
  return {
    callInfo: {
      fields:
        [{wazzup: {label:"Что случилось", type:"textarea"}}
        ,{callType: {label:"Тип звонка", data:"callTypes"}}
        ,{callerType:
          {label:"Кто звонит?"
          ,type:"options"
          ,data:
            ["Клиент"
            ,"Подрядчик"
            ,"Дилерский центр"
            ,"Заказчик программы"
            ,"Другое"
            ]
          , default:0}}
        ],
      dependencies:
        {callerType: {
          type:{append:"right"}, // "replace" "inline"
          value:
            ["generalContact"
            ,"selectContractor"
            ,"selectDealer"
            ,"generalContact"
            ,"generalContact"
            ]
        }}
    },
    generalContact: {
      fields:
        [{name: {label:"ФИО"}}
        ,{phone: {label:"Телефоны",validate:/^[+ ,;\d]*$/}}
        ]
    },
    selectCase: {
      fields:
        [{query:{label:"Поиск в кейсах"}}
        ,{cases:{type:"searchTable",query:"selectCase.query",data:"link to source"}}
        ]
    },
    selectContractor: {
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
              ,"Комментарий"]
            ,query:
              {"selectContractor.company":0
              ,"selectContractor.name":2
              ,"selectContractor.phones":3}
            ,source:"/api/search_contractors"
            }}
          }
        ]
    },
    selectDealer: {
      fields:
        [{company:{label:"Компания"}}
        ,{name:{label:"ФИО"}}
        ,{phones:{label:"Контактные телефоны"}}
        ,{save:{label:"Сохранить",type:"link"}}
        ,{dealer:{
          type:"searchTable",
          searchTable:
            {columns:
              ["Компания"
              ,"Город"
              ,"ФИО"
              ,"Должность"
              ,"Телефон"
              ]
            ,query:{"company":0,"name":2,"phones":4}
            ,source:"/api/search_dealers"
            }}
          }
        ]
    }
  };
}

