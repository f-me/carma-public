# Portal screen, derived from contract search screen
define [ "search/screen"
       , "text!tpl/screens/contract.html"
       , "model/main"
       , "utils"
       ], (Screen, tpl, main, u) ->
  # Initialize portal search screen from portal-stripped Contract
  # model
  screenConstructor = (Contract, Search, Table) ->
    # All portal fields marked with showtable option in subprogram
    # dictionary are searchable and shown in the table
    resultFields = _.map Table.fields, (f) ->
            name: f.name
            fixed: true
    searchFields = _.pluck resultFields, 'name'
    Screen.constructor
        noState: true
        hideFieldsList: true
        apiUrl: "/search/portal"
        searchModels: [Search]
        resultModels: [Contract]
        resultTable: _.filter resultFields, (f) -> f.name != "subprogram"
        searchFields: searchFields
        defaultSort: { fields: [{ model: "Contract", name: "ctime" }], order: "desc" }
        allowedResultFields:
          Contract: _.pluck Contract.fields, 'name'

  # Given subprogram id and its title, setup logo, title and dealer
  # help on page header
  logoSetup = (sid, title) ->
    $.getJSON "/_/SubProgram/#{sid}", (instance) ->
      if instance.logo
        attachmentId = instance.logo.split(':')?[1]
        main.modelSetup("attachment") "logo", {id: attachmentId}, {}
      else
        $("#logo").attr "src", null
      $("#help-program").text(title)
      $("#help-text").html(instance.dealerHelp)

  contractForm = "contract-form"

  redirect = (hash) -> window.location.hash = hash

  template: tpl
  constructor: (viewName, {sub: subprogram, id: id}) ->
    spgms = u.newComputedDict "portalSubPrograms"
    def_spgm = spgms.source[0]?.value

    # Current contract id
    contract = ko.observable null

    # Ensure that subprogram is set
    if subprogram?
      s = parseInt subprogram
      if _.isNumber s
        subprogram = s
        if id?
          i = parseInt id
          if _.isNumber i
            contract i

    # Redirect to default program when #contract is accessed
    unless _.isNumber subprogram
      redirect "contract/#{def_spgm}"
      return

    contractModel = "Contract?sid=#{subprogram}"

    findSame = (kvm, cb) ->
      return unless kvm.id()
      vin = kvm['vin']?()
      num = kvm['cardNumber']?()
      params = ["id=#{kvm.id()}"]
      params.unshift "vin=#{vin}" if vin
      params.unshift "cardNumber=#{num}" if num
      $.getJSON "/contracts/findSame?#{params.join('&')}", cb


    # Open a contract by its id. If id is null, setup an empty
    # contract form.
    openContract = (cid) ->
      $('a[href="#contract-tab"]').tab("show")
      if cid?
        $('#render-contract').attr(
          "href",
          "/renderContract?contract=#{cid}")
        main.modelSetup(contractModel) contractForm, {id: cid}, {}
      else
        main.modelSetup(contractModel) contractForm,
          # TODO Set committer on server
          {subprogram: subprogram, committer: parseInt global.user.meta.mid}, {}

      kvm = global.viewsWare[contractForm].knockVM

      kvm["id"].subscribe (i) -> redirect "contract/#{subprogram}/#{i}"

      # Role-specific permissions
      kvm['isActiveDisableDixi'](true)
      if _.find(global.user.roles, (r) -> r == global.idents("Role").partner)
        kvm['commentDisableDixi'](true)  if kvm['commentDisabled']
      if _.find(global.user.roles, (r) -> r == global.idents("Role").contract_admin)
        kvm['disableDixi'](true)

      # True if a duplicate contract caused user to not save the
      # contract
      dupe = false

      # Prevent on-off behaviour of dixi: once true, it's always
      # true (#1042)
      kvm["always_true"] = false
      kvm["dixi"].subscribe (v) ->
        if v
          kvm["always_true"] = true
        if kvm["always_true"] and !dupe
          kvm["dixi"] true

      unless kvm["dixi"]()
        # When creating new contracts, check contract duplicates upon
        # contract saving, ignoring first dixi update (when default
        # fields are first fetched)
        t = kvm["dixi"].subscribe (o) ->
          t.dispose()
          check = kvm["dixi"].subscribe (v) ->
            return if !v
            findSame kvm, (r) ->
              if _.isEmpty(r)
                dupe = false
                return
              txt = "За последние 30 дней уже были созданы контракты с " +
                    "таким же VIN или номером карты участника, их id: " +
                    "#{_.pluck(r, 'id').join(', ')}. Всё равно сохранить?"
              if confirm(txt)
                dupe = false
                check.dispose()
              else
                dupe = true
                kvm["dixi"](false)

    contract.subscribe (c) ->
      if _.isNumber(c)
        # Redirect to contract URL (does not cause actual reload due
        # to a hack in routes module)
        redirect "contract/#{subprogram}/#{c}"
        openContract c

    logoSetup subprogram, spgms.getLab subprogram

    # Open contract from URL
    openContract contract() if contract()?

    # Open contracts upon table row click
    $("#tbl").on "click", "tr", ->
      contract $(this).data("source")

    # Create new contracts
    $("#new-contract-btn").click () ->
      contract null
      openContract contract()

    $.getJSON "/cfg/model/#{contractModel}", (Contract) ->
      $.getJSON "/cfg/model/#{contractModel}&field=showtable&view=portalSearch", (Search) ->
        $.getJSON "/cfg/model/#{contractModel}&field=showtable", (Table) ->
          # Search subscreen
          searchVM = screenConstructor Contract, Search, Table
          searchVM.subprogram subprogram

          # Update URL&info when subprogram changes
          searchVM.subprogram.subscribe (s) ->
            # Default if the subprogram is erased
            if _.isNull s
              searchVM.subprogram def_spgm
            redirect "contract/#{searchVM.subprogram()}"

          # Make subprogram label bold
          $(".control-label label").first().css("font-weight", "bold")
