define  [ "utils"
        , "hooks/common"
        , "text!tpl/screens/supervisorOps.html"
        ], (utils, hook, tpl) ->
  this.setupSupervisorOpsScreen = (viewName, args) ->
    setTimeout ->
      $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
      $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

      t = $("#supervisorOps-table")
      return if t.hasClass("dataTable")
      dt = utils.mkDataTable t,
        bPaginate: false
        aoColumns: [{}, {}, {}, {}, {bVisible: false}]
        fnCreatedRow: (nRow, aData) ->
          tpl = $('#dictionary-many-field-template').html()
          $('td:eq(2)', nRow).html(
            Mustache.render tpl, userModel.fieldHash.boCities)
          $('td:eq(3)', nRow).html(
            Mustache.render tpl, userModel.fieldHash.boPrograms)
          ko.applyBindings aData[4], nRow


      $.getJSON "/usersDict", (us) ->
        dt.fnClearTable()
        rows = for u in us when /back/.test u.roles
          do (u) ->
            koUser =
              boCities: ko.observable u.boCities
              boPrograms: ko.observable u.boPrograms

            hook.dictManyHook userModel, koUser
            row =
              [ u.value, u.label
              , arrStr(koUser.boCitiesLocals())
              , arrStr(koUser.boProgramsLocals())
              , koUser]

            update = (fName) -> (val) ->
              $.ajax
                type: "PUT"
                url: "/userMeta/#{u.value}"
                data: "{\"#{fName}\": \"#{val}\"}"
              row[2] = arrStr(koUser.boCitiesLocals())
              row[3] = arrStr(koUser.boProgramsLocals())
              # i'm so ugly just to fliter out nonmatching rows
              dt.fnClearTable()
              dt.fnAddData rows

            koUser.boCities.subscribe (update 'boCities')
            koUser.boPrograms.subscribe (update 'boPrograms')
            row

        dt.fnAddData rows


  arrStr = (arr) -> (a.label for a in arr).join (" ")


  userModel =
    dictManyFields: ['boCities', 'boPrograms']
    fieldHash:
      boCities:
        name: 'boCities'
        meta: {dictionaryName: 'DealerCities'}
      boPrograms:
        name: 'boPrograms'
        meta: {dictionaryName: 'Programs'}

  { constructor: setupSupervisorOpsScreen
  , template: tpl
  }
