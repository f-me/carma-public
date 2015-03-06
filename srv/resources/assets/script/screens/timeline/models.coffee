define [], ->
  MassUpdate:
    name: "massUpdate"
    title: "Настройки пользователя"
    canUpdate: true
    fields: [
      {
        type: "dictionary"
        name: "businessRole"
        canWrite: true
        meta:
          dictionaryName: "BusinessRole"
          dictionaryType: "ModelDict"
          label: "Бизнес-роль"
      },
      {
        type: "dictionary-many"
        name: "bocities"
        canWrite: true
        meta:
          dictionaryName: "City"
          dictionaryType: "ModelDict"
          label: "Города"
          infoText: "Если ничего не выбрано, текущий набор будет стёрт"
      },
      {
        type: "dictionary-many"
        name: "boprograms"
        canWrite: true
        meta:
          dictionaryName: "Program"
          dictionaryStringify: true
          dictionaryType: "ModelDict"
          label: "Программы"
          infoText: "Если ничего не выбрано, текущий набор будет стёрт"
      },
    ]
