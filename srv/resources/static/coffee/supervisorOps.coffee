this.setupSupervisorOpsScreen = (viewName, args) ->
  setTimeout ->
    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

    t = $("#supervisorOps-table")
    return if t.hasClass("dataTable")
    dt = mkDataTable t,
      bPaginate: false
      aoColumns: [{}, {}, {}, {}, {bVisible: false}]
      fnCreatedRow: (nRow, aData) ->
        u = aData[4]
        koUser =
          boCities: ko.observable u.boCities
          boPrograms: ko.observable u.boPrograms

        koUser.boCities.subscribe (update u.value, 'boCities')
        koUser.boPrograms.subscribe (update u.value, 'boPrograms')
        dictManyHook userModel, koUser

        tpl = $('#dictionary-many-field-template').html()
        $('td:eq(2)', nRow).html(
          Mustache.render tpl, userModel.fieldHash.boCities
        )
        $('td:eq(3)', nRow).html(
          Mustache.render tpl, userModel.fieldHash.boPrograms
        )
        ko.applyBindings koUser, nRow

    $.getJSON "/usersDict", (us) ->
      dt.fnClearTable()
      rows = for u in us when /back/.test u.roles
        [u.value, u.label, "", "", u]
      dt.fnAddData rows


update = (login, fName) -> (val) ->
  $.ajax
    type: "PUT"
    url: "/userMeta/#{login}"
    data: "{\"#{fName}\": \"#{val}\"}"


userModel =
  dictManyFields: ['boCities', 'boPrograms']
  fieldHash:
    boCities:
      name: 'boCities'
      meta: {dictionaryName: 'DealerCities'}
    boPrograms:
      name: 'boPrograms'
      meta: {dictionaryName: 'Programs'}
