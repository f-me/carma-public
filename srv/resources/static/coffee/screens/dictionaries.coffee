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
      kvm = main.modelSetup(modelName) viewName, args, options
      kvm['updateUrl'] = ->
        global.router.navigate "dictionaries/#{modelName}/#{kvm.id()}",
                               { trigger: false }
      kvm

    visibleFields = ->
      [{name:'id', label:'#'}
      ,{name:'label', label:'Название'}
      ,{name:'value', label:'Значение'}]

    objsToRows = (instances) ->
      _.map instances, (inst) ->
        row = []
        _.each do visibleFields, (field) ->
          row.push(inst[field.name] || '')
        row

    screenSetup = (viewName, args) ->
      dicts = [{id: null, name: 'Выберите справочник' }]
      $.bgetJSON '/_/Dictionary', (ds) =>
        for d in ds
          dicts.push({id: d.id, name: d.description})
      ko.applyBindings(dicts, el("dict-select"))
      $('#dict-select').change ->
        if @value
          location.hash="dict/#{@value}"
          location.reload true

      if args.dict
        dictName = null
        $.bgetJSON "/_/Dictionary/#{args.dict}", (d) ->
          dictName = d.name

        kvm = modelSetup dictName, viewName, args

        tableHeader = _.reduce(do visibleFields, (memo, field) ->
          memo + "<th>#{field.label}</th>"
        , '')
        tableHeader = "<thead><tr>#{tableHeader}</tr></thead>"
        $("#dict-table").append(tableHeader)

        tableParams =
          tableName: "dict"
          objURL: "/_/#{dictName}"

        table = screenman.addScreen(dictName, -> )
          .addTable(tableParams)
          .setObjsToRowsConverter(objsToRows)
          .on("click.datatable", "tr", ->
            id = @children[0].innerText
            k = modelSetup dictName, viewName, {id}
            k['updateUrl']()
            k)

        screenman.showScreen dictName

        $('#permissions').find('.btn-success').on 'click', ->
          row = _.map do visibleFields, (field) ->
            kvm[field.name]?() || ''
          table.dataTable.fnAddData [row]

        $("#add-new-item-btn").on 'click', ->
          location.hash="#dict/#{args.dict}"
          location.reload true

        # setup 'show only active records' button
        hasActiveField = _.find kvm._meta.model.fields, (f) ->
          f.name is "active"

        if hasActiveField
          $("#active-items-btn").on 'click', ->
            objUrl = tableParams.objURL
            unless $(@).hasClass('active')
              objUrl = "#{objUrl}/?select=active==1"
            table.setObjs objUrl
          $("#active-items-btn").show()
        else
          $("#active-items-btn").hide()

    constructor: screenSetup
    template: tpl
