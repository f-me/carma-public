this.setupPartnersForm = (viewName, args) ->
  refs = [field: "services"
         ,forest: "partner-services-references"
         ]
  modelSetup("partner") viewName, args,
                        permEl: "partner-permissions"
                        focusClass: "focusable"
                        refs: refs

  $("#partner-add-service-container").html(
    Mustache.render $("#add-ref-button-template").html(),
            fn:    "addNewServiceToPartner();"
            label: "Добавить услугу"
  )

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
  bindRemove parent, 'services', k
  focusRef k
  # addTarifStuff i for i in parent['servicesReference']()

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
