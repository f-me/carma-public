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
        aoColumns: utils.repeat(5,null)
#        fnCreatedRow: (nRow, aData) ->
#          ko.applyBindings aData[5], nRow
      at = utils.newModelDict "ActionType", true
      $.getJSON "/_/Usermeta", (us) ->
       $.getJSON "/supervisor/opStats", (os) ->
        dt.fnClearTable()
        backRe = new RegExp(String(global.idents("Role").back))

        rows = for u in us when (backRe.test u.roles)
          do (u) ->
            login = u.login

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
                  cid = stats.caseId
                  [ "<a href=\"/#case/#{cid}\" target=\"_blank\">#{cid}</a>"
                  , at.getLab stats.aName
                  ]

              rowStats =
                [ caseLink
                , formattedTs
                , actionLabel
                ]
            else
              rowStats = [null, null, null]

            row =
              [ u.login, u.realName
              ].concat(rowStats)
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
  removeSupervisorOpsScreen = -> tick = false

  constructor: setupSupervisorOpsScreen
  destructor : removeSupervisorOpsScreen
  template: tpl
