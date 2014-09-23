define [
      "model/main"
    , "utils"
    , "sync/datamap"
    , "dictionaries"
    , "text!tpl/fields/form.html"
    ],
  (Main, Utils, DataMap, Dict, Flds) ->
    flds =  $('<div/>').append($(Flds))
    ko.bindingHandlers.renderContract =
      update: (el, acc, allBindigns, contract, ctx) ->
        title = ko.utils.unwrapObservable acc()
        expired = ""
        unless contract.isExpired() is undefined
          expired = if contract.isExpired()
              "<span class='label label-important'>Не действует</span>"
            else
              "<span class='label label-success'>Действует</span>"
        close = "<button title='Стереть из кейса ссылку на контракт'
                         class='close'><i class='icon-trash'/></button>"
        $(el).append("<legend>#{title} ##{contract.id()} #{expired} #{close}</legend>")
        $(el).find('.close').on 'click', -> contract.close()
        $dl = $("<table class='table table-condensed table-striped'></table>")
        $(el).append $dl
        _.each contract._meta.model.fields, (f) ->
          if f.type.indexOf("dictionary") != -1
            value = contract["#{f.name}Local"]()
          else
            value = contract[f.name]()

          if value and f.meta.label
            value = "<td>#{value}</td>"
            label = "<td><strong>#{f.meta.label}</strong></td>"
            $dl.append("<tr>#{label}#{value}</tr>")

    showContract = (contract, knockVM, el) ->
      contract.isActive = if contract.isActive then "Да" else "Нет"
      delete contract.dixi

      model = global.model 'Contract'
      mapper = new DataMap.Mapper(model)
      kvm = Main.buildKVM model, {fetched: mapper.s2cObj contract}

      kvm.isExpired = ko.computed ->
        return unless knockVM.callDate?()
        callDate = Date.parseExact(knockVM.callDate(), "dd.MM.yyyy HH:mm:ss")?.getTime()
        validSince = Date.parseExact(contract.validSince, "yyyy-MM-dd")?.getTime()
        validUntil = Date.parseExact(contract.validUntil, "yyyy-MM-dd")?.getTime()
        callDate < validSince or callDate > validUntil

      kvm.close = ->
        return unless confirm "Стереть из кейса ссылку на контракт?"
        knockVM.contract("");

      $("##{el}").html(
        Mustache.render($(flds).find("#contract-content-template").html(),
                       {title: "Контракт"}))
      ko.applyBindings(kvm, $("#contract-content")[0])

    hideContract = (el) ->
      $("##{el}").empty()

    fetchContract = (id) ->
      JSON.parse ($.bgetJSON "/_/Contract/#{id}").responseText


    setup: (el, kvm) ->
      # show linked contract if it there
      if kvm["contract"]?()
        showContract (fetchContract kvm["contract"]()), kvm, el

      # change contract view, when user choose another contract
      kvm["contract"]?.subscribe (id) ->
        if id
          showContract (fetchContract id), kvm, el
        else
          hideContract el
