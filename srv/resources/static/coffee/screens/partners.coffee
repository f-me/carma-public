define ["hooks/partner", "utils", "text!tpl/screens/partner.html"],
  (p, utils, tpl) ->
    setupPartnersForm = (viewName, args) ->
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

      # I need this object because I can't clean foreach binding, once
      # it's created, to use this proxy object to keep current partner's
      # allerts
      global.alertObj = { kvm: ko.observable(kvm)}
      ko.applyBindings(global.alertObj, $("#partner-errors")[0])
      setTimeout(->
        $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
        $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

        t = $("#partner-table");
        return if t.hasClass("dataTable")
        utils.mkDataTable(t)

        t.on("click.datatable", "tr", ->
          id = this.children[0].innerText
          kvm = modelSetup("partner") viewName, {"id": id},
                                permEl: "partner-permissions"
                                focusClass: "focusable"
                                refs: refs
          k = global.viewsWare['partner-view'].knockVM
          global.alertObj.kvm(kvm)

          k['servicesReference'].subscribe ->
            addTarifStuff i for i in k['servicesReference']()
        )
        dict = global.dictValueCache['DealerCities']
        $.getJSON("/allPartners",
            (objs) ->
                dt = t.dataTable()
                dt.fnClearTable()
                rows = for obj in objs
                    [obj.id
                    ,obj.name       || ''
                    ,dict[obj.city] || ''
                    ,obj.comment    || ''
                    ]
                dt.fnAddData(rows)
        ))

    addNewServiceToPartner = (name) ->
      p = global.viewsWare["partner-view"].knockVM
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
          p.bindRemove kvm, 'tarifOptions'
          focusRef k

    releasePartnersForm = () ->
      ko.cleanNode($("#partner-errors")[0])
      delete global.alertObj

    { constructor: setupPartnersForm
    , destructor : releasePartnersForm
    , template: tpl
    }