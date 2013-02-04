define ["utils"], (utils) ->
  this.setupRKCOpsScreen = (viewName, args) ->
    setTimeout ->

      eachao = $('#rkc-ops-back-operators-table')
      return if eachao.hasClass("dataTable")

      actstbl = {}
      actstbl.cols = for v in global.dictionaries.ActionNames.entries
          c =
              name: v.label

      actstbl.cols.unshift { name: "Среднее время обработки действия" }
      actstbl.cols.unshift { name: "Оператор" }

      ko.applyBindings(actstbl, el("rkc-ops-back-operators-table"))

      eat = utils.mkDataTable eachao, { bFilter: false, bInfo: false }

      fmttime = (tm) ->
          fmt = (x) -> if x < 10 then "0" + x else "" + x
          Math.floor(tm / 60) + ":" + fmt(tm % 60)

      fmtavg = (val) ->
          fmttime(val[0]) + "/" + val[1]

      getArgs = () -> this.filterRKCArgs()

      update = () ->
        args = getArgs()

        $.getJSON("/rkc" + args, (result) ->
          dict = global.dictValueCache
          eat.fnClearTable()

          eavision = []
          eavision.length = actstbl.cols.length
          eavision[0] = true
          eavision[1] = true

          earows = for eainfo in result.eachopactions
              r = for val, i in eainfo.avgs
                  if val
                      eavision[i + 2] = true
                  if val then fmtavg(val) else "-"
              r.unshift fmtavg(eainfo.avg)
              r.unshift eainfo.name
              earow = r

          for c, i in eavision
              eat.fnSetColumnVis(i, if c then true else false)

          eat.fnAddData(earows))

      partners = ko.observableArray([])
      this.initRKCDate update, partners
      this.fillRKCFilters update, partners

      global.rkcOpsData = {}

      global.rkcOpsData.updateHandler = setInterval(update, 30000)

      update()
      this.updatePartners(partners)

  this.removeRKCOpsScreen = ->
      t = global.rkcOpsData.updateHandler
      clearInterval t if t?

  { constructor: setupRKCOpsScreen, destructor: removeRKCOpsScreen }