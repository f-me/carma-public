define ["utils", "model/utils", "partnerCancel"], (u, mu, partnerCancel) ->
  partnerOptsHook: (i, knockVM) ->
    knockVM['contractor_partner'].subscribe (n) ->
      return unless knockVM['view']
      v = global.viewsWare[knockVM['view']].depViews['cost_counted'][0]
      $("##{v}").find(".add-opt-btn").remove()
      model = knockVM.modelName()
      u.sTout 1000, ->
        $.getJSON "/opts/#{knockVM.modelName()}/#{knockVM.id()}", (opts)->
          return if _.isEmpty opts
          tr = Mustache.render(
                $('#tarif-opt-sel-template').html(),
                opts:
                  for i in opts
                    { id: i.id
                    , optionName: (i.optionName || "Тарифная опция")}
          )
          $("##{v}").children().last().after(tr)
          $("##{v}").find('.reload').on 'click.reloadCountedCost', ->
            r = global.viewsWare['case-form'].knockVM['servicesReference']()
            o.model().fetch() for o in r
          $("##{v}").find('.add').on 'click.addTarif', ->
            s = $("##{v}").find("select")
            return if _.isEmpty s
            o = _.find opts, (opt) -> "#{opt.id}" == s.val()
            mu.addReference knockVM, 'cost_serviceTarifOptions',
              modelName: "cost_serviceTarifOption"
              args     :
                optionName   : o.optionName
                tarifOptionId: "tarifOption:#{o.id}"
              ->
                u.bindDelete knockVM, 'cost_serviceTarifOptions'
                r = knockVM['cost_serviceTarifOptionsReference']()
                $("##{(_.last r)['view']}").parent().collapse("show")
          u.bindDelete knockVM, 'cost_serviceTarifOptions'

  srvOptUpd: (instance, knockVM) ->
    knockVM['payType'].subscribe (n) ->
      u.sTout 500, ->
        for o in knockVM['cost_serviceTarifOptionsReference']()
          do (o) ->
            o.model().fetch()

  costsMark: (instance, knockVM) ->
    knockVM['marginalCost'].subscribe -> mbMark()

    knockVM['cost_counted'].subscribe -> mbMark()
    mbMark = ->
      v = knockVM.view
      mc = $("##{v}").find('[name=marginalCost]').parents('.control-group')
      cc = $("##{v}").find('[name=cost_counted]').parents('.control-group')
      mf = parseFloat(knockVM['marginalCost']())
      cf = parseFloat(knockVM['cost_counted']())
      if mf < cf
        mc.addClass('error')
        cc.addClass('error')
      else
        mc.removeClass('error')
        cc.removeClass('error')

  bindPartnerCancelDialog: (instance, knockVM) ->
    knockVM['showPartnerCancelDialog'] = ->
      partnerCancel.setup(knockVM.contractor_partnerId, knockVM.contractor_partner)


