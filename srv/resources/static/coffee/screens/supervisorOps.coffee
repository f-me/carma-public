define  [ "utils"
        , "hooks/common"
        , "text!tpl/screens/supervisorOps.html"
        ], (utils, hook, tpl) ->
  # data = { tick: true }
  tick = true
  setupSupervisorOpsScreen = (viewName, args) ->
    setTimeout ->
      $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
      $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

      t = $("#supervisorOps-table")
      return if t.hasClass("dataTable")

      dt = utils.mkDataTable t,
        bPaginate: false
        aoColumns: utils.repeat(7,null).concat [{bVisible: false}]
        fnCreatedRow: (nRow, aData) ->
          tpl = $('#dictionary-many-field-template').html()
          $('td:eq(2)', nRow).html(
            Mustache.render tpl, userModel.fieldHash.boCities)
          $('td:eq(3)', nRow).html(
            Mustache.render tpl, userModel.fieldHash.boPrograms)
          ko.applyBindings aData[7], nRow


      $.getJSON "/allUsers", (us) ->
       $.getJSON "/supervisor/opStats", (os) ->
        dt.fnClearTable()
        backRe = new RegExp(String(global.idents("Role").back))
        
        rows = for u in us when (backRe.test u.roles)
          do (u) ->
            koUser =
              boCities: ko.observable u.boCities
              boCitiesDisabled: ko.observable false
              boPrograms: ko.observable u.boPrograms
              boProgramsDisabled: ko.observable false

            hook.dictManyHook userModel, koUser
            login = u.value

            stats = os.stats[login]
            if stats              
              [idle, [formattedTs, ts]] =
                if _.isEmpty stats.closeTime
                  [false, (utils.timeFrom stats.openTime, os.reqTime)]
                else
                  [true, (utils.timeFrom stats.closeTime, os.reqTime)]
                  
              [caseLink, actionLabel] =
                if idle
                  ["нет", null]
                else
                  cid = stats.caseId.split(':')[1]
                  [ "<a href=\"/#case/#{cid}\" target=\"_blank\">#{cid}</a>"
                  , global.dictValueCache['ActionNames'][stats.aName]
                  ]

              rowStats =
                [ caseLink
                , formattedTs
                , actionLabel
                ]
            else
              rowStats = [null, null, null]

            row =
              [ u.value, u.label
              , arrStr(koUser.boCitiesLocals())
              , arrStr(koUser.boProgramsLocals())
              ].concat(rowStats).concat([koUser])

            update = (fName) -> (val) ->
              $.ajax
                type: "PUT"
                url: "/_/usermeta/#{u.mid}"
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
        updateBusy()

  updateBusy = ->
    $.getJSON "/supervisor/busyOps", (d) ->
      ops = {}
      for i in d
        ops[i.login] = i.count
      $("#supervisorOps-table tr").each (i,e) ->
        $(e).children('td').css('background-color', '')
        if ops[ $($(e).find('td')[0]).text() ] > 5
          $(e).children('td').css('background-color', '#FF8888')
      setTimeout updateBusy, 5000 if tick

  arrStr = (arr) -> (a.label for a in arr).join (" ")


  userModel =
    dictManyFields: ['boCities', 'boPrograms']
    fields: [
      { name: 'boCities'
      , meta: {dictionaryName: 'DealerCities'}
      , type: "dictionary-many" },
      { name: 'boPrograms'
      , meta: {dictionaryName: 'Programs'}
      , type: "dictionary-many"
      } ]

  userModel['fieldHash'] = {}
  for f in userModel.fields
    userModel['fieldHash'][f.name] = f

  removeSupervisorOpsScreen = -> tick = false

  constructor: setupSupervisorOpsScreen
  destructor : removeSupervisorOpsScreen
  template: tpl
