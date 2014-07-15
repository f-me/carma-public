define [ "utils"
       , "text!tpl/screens/user.html"
       , "model/utils"
       , "model/main"
       ],
  (utils, tpl, mu, main) ->
    setupUsersForm = (viewName, args) ->
      kvm = main.modelSetup("usermeta") viewName, args,
                            permEl: "user-permissions"
                            focusClass: "focusable"
                            manual_save: true

      $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
      $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

      t = $("#user-table");
      return if t.hasClass("dataTable")
      utils.mkDataTable(t)

      # Add empty table row when a new user starts to be created
      kvm['maybeId'].subscribe ->
        t.dataTable().fnAddData [[kvm['id'](), kvm['login'](), kvm['realName']()]]

      # Show user form when a table row is clicked
      t.on("click.datatable", "tr", ->
        id = this.children[0].innerText
        kvm = main.modelSetup("usermeta") viewName, {"id": id},
                              permEl: "user-permissions"
                              focusClass: "focusable"
                              manual_save: true
      )

      # Populate the table
      $.getJSON("/allUsers",
          (objs) ->
              dt = t.dataTable()
              dt.fnClearTable()
              rows = for obj in objs
                  [ obj.mid
                  , obj.value || ''
                  , obj.label || ''
                  ]
              dt.fnAddData(rows)
      )

    { constructor: setupUsersForm
    , template: tpl
    }
