define [ "utils"
       , "text!tpl/screens/dictionaries.html"
       , "model/utils"
       , "model/main"
       , "screenman"
       ],
  (utils, tpl, mu, main, screenman) ->

    modelSetup = (modelName, viewName, args) ->
      permEl = "permissions"
      focusClass = "focusable"
      refs = []
      options = {permEl, focusClass, refs}
      main.modelSetup(modelName) viewName, args, options

    visibleFields = ->
      [{name:'id', label:'#'}
      ,{name:'label', label:'Название'}
      ,{name:'value', label:'Значение'}]

    objsToRows = (instances) ->
      _.map instances, (inst) ->
        row = []
        _.each do visibleFields, (field) ->
          fieldValue = inst[field.name] || ''
          switch field.name
            when 'id' then row.push fieldValue.split(':')[1]
            else row.push fieldValue
        row

    showProgramModelComponents = ->
      $("#program-model-components").removeClass("hide")
      $("#program-add-programPermissions-container").html(
        Mustache.render $("#add-ref-button-template").html(),
                fn:    "addNewPermissionToProgram();"
                label: "Добавить ограничение на поле контракта"
      )

      afterAddSrv = (parent) -> (k) -> utils.focusRef k

      this.addNewPermissionToProgram = (name) ->
        p = global.viewsWare["dictionaries-view"].knockVM
        mu.addReference p,
                     'programPermissions',
                     {modelName: 'programPermissions'},
                     afterAddSrv(p)


    screenSetup = (viewName, args) ->

      dictName = args.dict

      if dictName
        kvm = modelSetup dictName, viewName, args

        tableHeader = _.reduce(do visibleFields, (memo, field) ->
          memo + "<th>#{field.label}</th>"
        , '')
        tableHeader = "<thead><tr>#{tableHeader}</tr></thead>"
        $("#dict-table").append(tableHeader)

        tableParams =
          tableName: "dict"
          objURL: "/all/#{dictName}"

        table = screenman.addScreen(dictName, -> )
          .addTable(tableParams)
          .setObjsToRowsConverter(objsToRows)
          .on("click.datatable", "tr", ->
            id = @children[0].innerText
            modelSetup dictName, viewName, {id}
            global.viewsWare["dictionaries-view"].knockVM)
        screenman.showScreen dictName

        $('#permissions').find('.btn-success').on 'click', ->
          row = _.map do visibleFields, (field) ->
            kvm[field.name]?() || ''
          table.dataTable.fnAddData [row]

        $("#add-new-item-btn").on 'click', ->
          location.hash="#dictionaries/#{dictName}"
          location.reload true

        if (dictName is 'program')
          do showProgramModelComponents

    constructor: screenSetup
    template: tpl
