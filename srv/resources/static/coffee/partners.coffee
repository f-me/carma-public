this.setupPartnersForm = (viewName, args) ->
  refs = [field: "services"
         ,forest: "partner-services-references"
         ]
  kvm = modelSetup("partner") viewName, args,
                        permEl: "partner-permissions"
                        focusClass: "focusable"
                        refs: refs

  $("#partner-add-service-container").html(
    Mustache.render $("#add-ref-button-template").html(),
            fn:    "addNewServiceToPartner();"
            label: "Добавить услугу"
  )

  ko.cleanNode($("#partner-errors")[0])
  ko.applyBindings(kvm, $("#partner-errors")[0])
  setTimeout(->
    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

    t = $("#partner-table");
    return if t.hasClass("dataTable")
    mkDataTable(t)

    t.on("click.datatable", "tr", ->
      id = this.children[0].innerText
      modelSetup("partner") viewName, {"id": id},
                            permEl: "partner-permissions"
                            focusClass: "focusable"
                            refs: refs
      k = global.viewsWare['partner-form'].knockVM
      k['servicesReference'].subscribe ->
        addTarifStuff i for i in k['servicesReference']()
    )

    $.getJSON("/all/partner?fields=id,name,city,comment",
        (objs) ->
            dt = t.dataTable()
            dt.fnClearTable()
            rows = for obj in objs
                [obj.id.split(':')[1]
                ,obj.name || ''
                ,obj.city || ''
                ,obj.comment || ''
                ]
            dt.fnAddData(rows)
    ))

this.addNewServiceToPartner = (name) ->
  p = global.viewsWare["partner-form"].knockVM
  addReference p,
               'services',
               {modelName: 'partner_service'},
               afterAddSrv(p)

afterAddSrv = (parent) -> (k) ->
  focusRef k

addTarifStuff = (p) ->
  view = $("##{p['view']}")
  button = Mustache.render $("#add-ref-button-template").html(),
    fn:    ""
    label: "Добавить опцию"
  view.children().last().after button
  view.children().last().click -> genNewTarif p

genNewTarif = (kvm) ->
  addReference kvm, 'tarifOptions', { modelName: 'tarifOption' },
    (k) ->
      bindRemove kvm, 'tarifOptions'
      focusRef k

this.bindTitleServiceName = (instance, kvm) ->
  kvm['modelTitle'] = kvm['serviceNameLocal']

this.bindRemoveService = (instance, kvm) ->
  kvm['services'].subscribe -> bindRemove kvm, 'services'

this.serviceRepeat = (instance, kvm) ->
  kvm['serviceRepeat'] = ko.observable([])
  kvm['services'].subscribe -> setServiceRepeat()

this.partnerServiceRepeat = (instance, kvm) ->
  kvm['serviceName'].subscribe -> setServiceRepeat()

this.setServiceRepeat = ->
  kvm = global.viewsWare['partner-form'].knockVM
  return unless kvm
  refs = _.filter kvm.servicesReference(), (r) -> r.serviceName()
  groups = _.groupBy refs, (r) -> r.serviceName()
  r = (v[0].serviceNameLocal() for k, v of groups when v.length > 1)
  kvm.serviceRepeat(r)
