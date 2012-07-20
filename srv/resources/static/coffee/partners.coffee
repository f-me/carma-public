this.setupPartnersForm = (viewName, args) ->
  refs = [field: "services"
         ,forest: "partner-service-references"
         ]
  modelSetup("partner") viewName, args,
                        permEl: "partner-permissions"
                        focusClass: "focusable"
                        refs: refs

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
  instance = global.viewsWare["partner-form"].bbInstance
  book = addReference instance,
                 field     : "services"
                 modelName : "partner_service"
                 forest    : "partner-service-references",
                 "center"
  service = global.dictionaries.Services
