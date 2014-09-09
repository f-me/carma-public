define [], ->
  PartnerSearch:
    name: "partnerSearch"
    title: "Экран поиска партнеров"
    fields: [
      { name: "search"
      , meta: { label: "Поиск", nosearch: true }
      },
      { name: "city"
      , type: "dictionary-set-int"
      , meta:
          dictionaryName: "City"
          dictionaryType: "ModelDict"
          dictionaryStringify: true
          label: "Город"
      },
      { name: "make"
      , type: "dictionary-set-int"
      , meta:
          widget: "dictionary-many"
          dictionaryName: "CarMake"
          dictionaryType: "ModelDict"
          dictionaryStringify: true
          label: "Марка"
      },
      { name: "services"
      , type: "dictionary-set-int"
      , meta:
          dictionaryName: "ServiceType"
          dictionaryType: "ModelDict"
          dictionaryStringify: true
          label: "Услуги"
      },
      { name: "priority2"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Priorities"
          dictionaryType: "ComputedDict"
          bounded: false
          label: "ПБГ"
      },
      { name: "priority3"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Priorities"
          dictionaryType: "ComputedDict"
          bounded: false
          label: "ПБЗ"
      },
      { name: "isDealer"
      , type: "checkbox"
      , meta: { label: "Дилер" }
      },
      { name: "mobilePartner"
      , type: "checkbox"
      , meta: { label: "Мобильный партнер" }
      },
      { name: "workNow"
      , type: "checkbox"
      , meta: { label: "Работают сейчас", nosearch: true}
      }
      # Case coordinates, stored as "lon,lat" text in WSG projection
      { name: "coords"
      , meta: { label: "Координаты места поломки", nosearch: true}
      },
      { name: "address"
      , meta: { label: "Адрес места поломки", nosearch: true}
      }
    ]

  SearchResults:
    name: "Результаты поиска"
    fields: [
      { name: "search"
      , meta: { label: "Поиск" }
      },
      { name: "name"
      , meta: { label: "Название" }
      },
      { name: "city"
      , type: "dictionary"
      , meta: {
          label: "Город"
          dictionaryName: "City"
          dictionaryType: "ModelDict"
        }
      },
      { name: "isfree"
      , meta: { label: "Свободен?" }
      },
      { name: "stale"
      , meta: { label: "Последнее обновление координат больше часа назад" }
      },
      { name: "ico"
      , meta: { label: "Иконка" }
      },
      { name: "ismobile"
      , meta: { label: "Мобильный партнер" }
      },
      { name: "isdealer"
      , meta: { label: "Мобильный партнер" }
      },
      { name: "distance"
      , meta: { label: "Расстояние" }
      },
      { name: "comment"
      , meta: { label: "Комментарий" }
      },
      { name: "phone"
      , meta: { label: "Телефон" }
      },
      { name: "personincharge"
      , meta: { label: "Ответственное лицо" }
      },
      { name: "workingTime"
      , meta: { label: "Рабочее Время" }
      },
      { name: "code"
      , meta: { label: "Код?" }
      },
      { name: "addrDeJure"
      , meta: { label: "Юридический адрес" }
      },
      { name: "addrDeFacto"
      , meta: { label: "Фактический адрес" }
      },
      { name: "st_x"
      , meta: { label: "" }
      },
      { name: "st_y"
      , meta: { label: "" }
      },
      { name: "addrs"
      , type: "json"
      , meta:
          label: "Адреса"
          jsonSchema: "dict-objects"
          dictionaryName: "AddressTypes"
      },
      { name: "phones"
      , type: "json"
      , meta:
          label: "Телефоны"
          jsonSchema: "dict-objects"
          dictionaryName: "PhoneTypes"
          noteLabel: "Время работы"
          showNote: true
          regexp: "phone"
      },
      { name: "emails"
      , type: "json"
      , meta:
          label: "E-mail"
          jsonSchema: "dict-objects"
          dictionaryName: "EmailTypes"
      },
      { name: "makes"
      , type: "dictionary-set-int"
      , meta:
         label: "Марки"
         widget: "dictionary-many"
         dictionaryName: "CarMake"
         dictionaryType: "ModelDict"
         dictionaryStringify: true
      },
      { name: "services"
      , type: "nested-model"
      , meta: {
        , label: "Услуги"
        , modelName: "PartnerServices"
        }
      }
    ]

  PartnerServices:
    name: "Результаты поиска"
    fields: [
      {
        meta:
          dictionaryName: "ServiceType"
          dictionaryType: "ModelDict"
          dictionaryStringify: true
          label: "Услуга"
        type: "dictionary",
        name: "servicename"
      },
      {
        meta:
          label: "Приоритет по безналу город"
        name: "priority2"
      },
      {
        meta:
          label: "Приоритет по безналу за город"
        name: "priority3"
      }
    ]
