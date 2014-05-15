define [
      "model/main"
    , "utils"
    , "sync/datamap"
    , "dictionaries"
    ],
  (Main, Utils, DataMap, Dict) ->

    ko.bindingHandlers.renderContract =
      update: (el, acc, allBindigns, contract, ctx) ->
        title = ko.utils.unwrapObservable acc()
        expired = ""
        unless contract.isExpired() is undefined
          expired = if contract.isExpired()
              "<span class='label label-important'>Просрочен</span>"
            else
              "<span class='label label-success'>Действует</span>"
        close = "<button class='close'>&times;</button>"
        $(el).append("<legend>#{title} ##{contract.id()} #{expired} #{close}</legend>")
        $(el).find('.close').on 'click', -> contract.close()
        $dl = $("<table class='table table-condensed table-striped'></table>")
        $(el).append $dl
        _.each contract._meta.model.fields, (f) ->
          if contract[f.name]() and f.meta.label
            label = "<td><strong>#{f.meta.label}</strong></td>"
            value = "<td>#{contract[f.name]()}</td>"
            $dl.append("<tr>#{label}#{value}</tr>")

    showContract = (contract, knockVM, el) ->
      if contract.make
        carMakeDict = new Dict.dicts.ModelDict
          dict: 'CarMake'
        contract.make = carMakeDict.getLab contract.make

      if contract.model
        carModelDict = new Dict.dicts.ModelDict
          dict: 'CarModel'
        contract.model = carModelDict.getLab contract.model

      if contract.subprogram
        subprogramDict = Utils.newComputedDict "prefixedSubPrograms"
        contract.subprogram = subprogramDict.getLab contract.subprogram

      if contract.committer
        usersDict = new Dict.dicts.ModelDict
          dict: 'Usermeta'
          meta:
            dictionaryKey: 'id'
            dictionaryLabel: 'realName'
        contract.committer = usersDict.getLab contract.committer

      contract.isActive = if contract.isActive then "Да" else "Нет"

      if contract.seller or contract.lastCheckDealer
        carSellerDict = new Dict.dicts.ModelDict
          dict: 'Partner'
          meta:
            dictionaryKey: 'id'
            dictionaryLabel: 'name'
        contract.seller = carSellerDict.getLab contract.seller
        contract.lastCheckDealer = carSellerDict.getLab contract.lastCheckDealer

      if contract.model
        carModelDict = new Dict.dicts.ModelDict
          dict: 'CarClass'
        contract.carClass = carModelDict.getLab contract.carClass

      if contract.model
        carModelDict = new Dict.dicts.ModelDict
          dict: 'CheckType'
        contract.checkType = carModelDict.getLab contract.checkType

      if contract.model
        carModelDict = new Dict.dicts.ModelDict
          dict: 'Transmission'
        contract.transmission = carModelDict.getLab contract.transmission

      if contract.model
        carModelDict = new Dict.dicts.ModelDict
          dict: 'Engine'
        contract.engineType = carModelDict.getLab contract.engineType

      if contract.model
        carModelDict = new Dict.dicts.ModelDict
          dict: 'LegalForm'
        contract.legalForm = carModelDict.getLab contract.legalForm

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
        knockVM.contract("");

      $("##{el}").html(
        Mustache.render($("#contract-content-template").html(), {title: "Контракт"}))
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
