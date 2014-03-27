define [ "utils"
       , "text!tpl/screens/dictionaries.html"
       , "model/utils"
       , "model/main"
       , "screenman"
       , "dictionaries/model-dict"
       ],
  (utils, tpl, mu, main, screenman, modelDict) ->

    textarea2wysiwyg = ->
      # transform all textarea elements to wisiwyg
      $("textarea").each (index) ->
        id = "wisyhtml5-" + index
        $(@).attr("id", id).wysihtml5
          image: false,
          html: true,
          locale: "ru-RU",
          events:
            blur: ->
              $("#" + id).trigger 'change'

    modelSetup = (dict, viewName, args) ->
      permEl = "permissions"
      focusClass = "focusable"
      refs = []
      options = {permEl, focusClass, refs}
      kvm = main.modelSetup(dict.name) viewName, args, options
      kvm['updateUrl'] = ->
        # FIXME: Drop this, because it makes filters unusable
        # Finch.navigate "dict/#{dict.id}/#{kvm.id()}", true
      kvm

    majorFieldsSetup = (dict, dictModel) ->
      majorFields = [{name:'id', label:'#', tr: (v) -> v}]
      for f in dict.majorFields
        fDesc = _.find dictModel.fields, (mf) -> mf.name == f
        if fDesc && fDesc.type != 'ident'
          tr = ->
            if fDesc.type == 'dictionary'
              d = new modelDict.dict {dict: fDesc.meta.dictionaryName}
              (v) -> d.getLab v
            else
              (v) -> v
          majorFields.push {name: f, label: fDesc.meta.label, tr: tr()}
      majorFields

    tableSetup = (objURL, majorFields, dict, viewName) ->
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
        objURL: objURL

      table = screenman.addScreen(dict.name, -> )
        .addTable(tableParams)
        .setObjsToRowsConverter(objsToRows)
        .on("click.datatable", "tr", ->
          id = @children[0].innerText
          k = modelSetup dict, viewName, {id}
          textarea2wysiwyg()
          k['updateUrl']()
          k)
      screenman.showScreen dict.name
      table

    setupButtonPanel = (kvm, table, args, objUrl) ->
      $("#button-panel").show()

      $("#add-new-item-btn").on 'click', ->
        location.hash="#dict/#{args.dict}"
        location.reload true

      # setup 'show only active records' button
      hasActiveField = _.find kvm._meta.model.fields, (f) ->
        f.name is "active"

      if hasActiveField
        $("#active-items-btn").on 'click', ->
          unless $(@).hasClass('active')
            objUrl = "#{objUrl}/?select=active==1"
          table.setObjs objUrl
        $("#active-items-btn").show()
      else
        $("#active-items-btn").hide()


    screenSetup = (viewName, args) ->
      # show choose dict controls
      dicts = [{id: null, name: 'Выберите справочник' }]
      $.bgetJSON '/_/Dictionary', (ds) =>
        for d in ds
          dicts.push({id: d.id, name: d.description})
      ko.applyBindings(dicts, el("dict-select"))
      $('#dict-select').change ->
        if @value
          location.hash="dict/#{@value}"
          location.reload true

      # if dict choosed
      if args.dict
        $('#dict-select').val(args.dict)

        # get dict model
        dict = null
        $.bgetJSON "/_/Dictionary/#{args.dict}", (d) -> dict = d
        dictName = dict.name
        dictModel = global.model dictName

        majorFields = majorFieldsSetup dict, dictModel
        table = null
        objURL = "/_/#{dictName}?limit=1000"

        # func to init dict table and controls to edit selected entry
        initEditControls = (objURL) ->
          table = tableSetup objURL, majorFields, dict, viewName
          kvm = modelSetup dict, viewName, args

          $('#permissions').find('.btn-success').on 'click', ->
            row = _.map majorFields, (field) -> kvm[field.name]?() || ''
            id = row[0]
            if _.every(table.dataTable.fnGetData(), (x) -> x[0] != id)
              table.dataTable.fnAddData [row]

          setupButtonPanel kvm, table, args, objURL
          textarea2wysiwyg()

        # let user show entries from a particular parent
        parentModel = global.model dictName, "parents"
        if not parentModel
          initEditControls objURL
        else
          parentKVM = main.buildKVM parentModel, {}
          parentKVM.find = =>
            filterParams = []
            for f in parentModel.fields
              val = parentKVM[f.name]()
              filterParams = filterParams.concat("#{f.name}=#{val}") if val

            objURL = "/_/#{dictName}?#{filterParams.join '&'}"
            if table
              table.setObjs objURL
            else
              initEditControls objURL

      # show parent select controls
      # I know what parentKVM may be 'undefined' here
      # it must be so if dict doesn't have parents
      ko.applyBindings {kvm: parentKVM}, el("dict-parent")

    constructor: screenSetup
    template: tpl
