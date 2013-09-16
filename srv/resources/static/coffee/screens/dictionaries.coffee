define [ "utils"
       , "text!tpl/screens/dictionaries.html"
       , "model/utils"
       , "model/main"
       , "screenman"
       , "lib/model-dict"
       ],
  (utils, tpl, mu, main, screenman, modelDict) ->

    modelSetup = (dict, viewName, args) ->
      permEl = "permissions"
      focusClass = "focusable"
      refs = []
      options = {permEl, focusClass, refs}
      kvm = main.modelSetup(dict.name) viewName, args, options
      kvm['updateUrl'] = ->
        global.router.navigate "dict/#{dict.id}/#{kvm.id()}",
                               { trigger: false }
      kvm

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
        dict = null
        majorFields = null
        $.bgetJSON "/_/Dictionary/#{args.dict}", (d) -> dict = d
        dictName = dict.name
        dictModel = global.model dictName

        majorFields = []
        for f in dict.majorFields
          fDesc = _.find dictModel.fields, (mf) -> mf.name == f
          if fDesc
            tr = (v) -> v
            if fDesc.type == 'dictionary' and f != 'id'
              d = new modelDict.dict {dict: fDesc.meta.dictionaryName}
              tr = (v) -> d.getLab v
            majorFields.push {name: f, label: fDesc.meta.label, tr: tr}

        kvm = modelSetup dict, viewName, args

        tableHeader = _.map(
            _.pluck(majorFields, 'label'),
            (l) -> "<th>#{l}</th>")
        tableHeader = "<thead><tr>#{tableHeader.join('')}</tr></thead>"
        $("#dict-table").append(tableHeader)

        objsToRows = (objs) ->
          _.map objs, (obj) ->
            for f in majorFields
              f.tr(obj[f.name]) || ''

        tableParams =
          tableName: "dict"
          objURL: "/_/#{dictName}"

        table = screenman.addScreen(dictName, -> )
          .addTable(tableParams)
          .setObjsToRowsConverter(objsToRows)
          .on("click.datatable", "tr", ->
            id = @children[0].innerText
            k = modelSetup dict, viewName, {id}
            k['updateUrl']()
            k)

        screenman.showScreen dictName

        $('#permissions').find('.btn-success').on 'click', ->
          row = _.map majorFields, (field) -> kvm[field.name]?() || ''
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
