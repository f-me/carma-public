{$, _, ko, Finch} = require "carma/vendor"

utils       = require "carma/utils"
mu          = require "carma/model/utils"
main        = require "carma/model/main"
{screenMan} = require "carma/screenman"
modelDict   = require "carma/dictionaries/model-dict"

template = require "carma-tpl/screens/dictionaries.pug"
Flds     = require "carma-tpl/fields/form.pug"

modelSetup = (dict, viewName, args) ->
  focusClass = "focusable"
  refs = []
  manual_save = true if dict.name is "Usermeta"

  kvm = main.modelSetup(dict.name) viewName, args, {
    focusClass
    refs
    manual_save
    slotsee: ["permissions"]
  }

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

  table = screenMan
    .addScreen dict.name, (->)
    .addTable tableParams
    .setObjsToRowsConverter objsToRows
    .setDataTableOptions aoColumnDefs: [{sWidth: "10%", aTargets: [0]}]
    .on "click.datatable", "tr", ->
      id = @children[0].innerText
      kvm = modelSetup dict, viewName, {id}
      do kvm['updateUrl']
      kvm

  screenMan.showScreen dict.name
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

  if kvm._meta.model.name != 'ConstructorFieldOption'
    $("#add-new-item-btn").show()
    $("#copy-options-btn").hide()
  else
    $("#active-items-btn").hide()
    $("#add-new-item-btn").hide()
    $("#copy-options-btn").show()
    copyModel =
      name: 'CopyCtrOptions'
      fields: [
        {name: 'from'
        ,type: 'dictionary'
        ,meta:
          bounded: true
          dictionaryType: 'ModelDict'
          dictionaryName: 'Program'
          label: 'Из программы'
        },
        {name: 'to'
        ,type: 'dictionary'
        ,meta:
          bounded: true
          dictionaryType: 'ModelDict'
          dictionaryName: 'Program'
          label: 'В программу'
        }]
    copyKVM = main.buildKVM copyModel, {}
    copyKVM.canCopy = ko.computed((-> @from() and @to() and @from() != @to()), copyKVM)
    copyKVM.doCopy = ->
      $.ajax
        type     : 'POST'
        url      : "/copyCtrOptions?from=#{copyKVM.from()}&to=#{copyKVM.to()}"
      $('#ctr-copy-modal').modal 'hide'

    ko.applyBindings {kvm: copyKVM}, el("ctr-copy-modal")


screenSetup = (viewName, args) ->
  # ad-hoc fix for broken z-index
  # TODO refactor
  $("#injected-modal").html ""
  $("#ctr-copy-modal").appendTo "#injected-modal"

  # FIXME: remove this hack when custom ko handler will be made with rjs
  $("#hidden-fields-container").append($(Flds))
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
    dictModel = window.global.model dictName

    majorFields = majorFieldsSetup dict, dictModel
    table = null
    objURL = "/_/#{dictName}?limit=4000" # TODO FIXME we urged to refactor this!

    # func to init dict table and controls to edit selected entry
    initEditControls = (objURL) ->
      table = tableSetup objURL, majorFields, dict, viewName
      kvm = modelSetup dict, viewName, args

      $('#permissions').find('.btn-success').on 'click', ->
        row = _.map majorFields, (field) ->
          kvm["#{field.name}Local"]?() or
            kvm[field.name]?() or ''
        id = row[0]
        if _.every(table.dataTable.fnGetData(), (x) -> x[0] != id)
          table.dataTable.fnAddData [row]

      setupButtonPanel kvm, table, args, objURL

    # let user show entries from a particular parent
    parentModel = window.global.model dictName, "parents"
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

module.exports = {
  constructor: screenSetup
  template
}
