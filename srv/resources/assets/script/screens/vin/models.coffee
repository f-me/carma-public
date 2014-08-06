define [], ->
  VinUpload:
    name: "vinUpload"
    title: "Загрузка VIN"
    canUpdate: true
    fields: [
      {
        type: "dictionary"
        name: "subprogram"
        canWrite: true
        meta:
          dictionaryName: "portalSubPrograms"
          dictionaryType: "ComputedDict"
          label: "Подпрограмма"
      },
      {
        type: "dictionary"
        name: "format"
        canWrite: true
        meta:
          dictionaryName: "VinFormat"
          dictionaryType: "ModelDict"
          label: "Формат"
      }
    ]
