{$, ko, _} = require "carma/vendor"

utils       = require "carma/utils"
mu          = require "carma/model/utils"
main        = require "carma/model/main"
{screenMan} = require "carma/screenman"

template = require "carma-tpl/screens/partner.pug"

modelSetup = (modelName, viewName, args) ->
  main.modelSetup(modelName) viewName, args,
    focusClass: "focusable"
    refs: [{field: "services", forest: "partner-services-references"}]
    slotsee: ["map-address"
              "#{modelName}-permissions"]

objsToRows = (objs) ->
  cities = utils.newModelDict "City", true
  # First two columns are hidden and used only for filtering
  rows = for obj in objs then [
    (if obj.isActive then "yes" else "no")
    (if obj.isDealer
        "dealer"
      else if obj.isMobile then "mobile" else "partner"
    )
    obj.id
    obj.name                or ''
    cities.getLab(obj.city) or ''
    obj.comment             or ''
  ]

screenSetup = (viewName, args) ->
  modelName = "Partner"

  # ad-hoc fix for broken z-index
  # TODO geo modals must be rewritten
  $("#injected-modal").html ""
  $("#partnerMapModal").appendTo "#injected-modal"

  kvm = modelSetup modelName, viewName, args

  tableParams =
    tableName : "partner"
    objURL    : "/_/Partner"

  table = screenMan
    .addScreen modelName, (->)
    .addTable tableParams
    .setObjsToRowsConverter objsToRows
    .setDataTableOptions(
      # We use first two columns to filter by partner's activity and type.
      columnDefs: [
        targets: [ 0, 1 ]
        visible: false
      ]
    )

  setupTableFilters "partner-table-filters", table

  table
    .on "click.datatable", "tr", ->
      if table.dataTable.fnGetPosition(this) isnt null
        id = @children[0].innerText
        modelSetup modelName, viewName, {id}

  screenMan.showScreen modelName

  $('#partner-permissions').find('.btn-success').on 'click', ->
    addrDeFacto = _.filter kvm["addrsObjects"](), (svm) -> svm.key() is "fact"

    obj =
      addrDeFacto : addrDeFacto[0]?.value()
      city        : kvm.city()
      comment     : kvm.comment()
      id          : kvm.id()
      isActive    : kvm.isActive()
      isDealer    : kvm.isDealer()
      isMobile    : kvm.isMobile()
      name        : kvm.name()

    table.dataTable.fnAddData objsToRows [obj]


setupTableFilters = (elem, table) ->
  filterModel =
    fields: [
        name: "isActive"
        type: "dictionary"
        meta:
            dictionaryName: "PartnerIsActive"
            dictionaryType: "ComputedDict"
      ,
        name: "partnerType"
        type: "dictionary"
        meta:
          dictionaryName: "PartnerType"
          dictionaryType: "ComputedDict"
    ]
  filterKvm = main.buildKVM(filterModel, elem)
  filterKvm.isActive.subscribe (v) ->
    table.dataTable.fnFilter (if v == "all" then "" else v), 0
  filterKvm.partnerType.subscribe (v) ->
    table.dataTable.fnFilter (if v == "all" then "" else v), 1

  ko.applyBindings filterKvm, document.getElementById(elem)


module.exports = {
  constructor: screenSetup
  template
}
