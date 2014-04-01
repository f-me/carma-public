# Portal screen, derived from contract search screen
define [ "search/screen"
       , "json!/cfg/model/Contract"
       , "json!/cfg/model/Contract?view=portalSearch"
       , "text!tpl/screens/contract.html"
       , "model/main"
       , "utils"
       ], (Screen, Contract, Search, tpl, main, u) ->
  screenConstructor = (res) ->
    resultFields = _.map res.fields, (f) ->
            name: f.name
            fixed: true
    searchFields = _.pluck resultFields, 'name'
    # Always search by subprogram
    unless _.contains searchFields, "subprogram"
      searchFields.push "subprogram"
    Screen.constructor
        noState: true
        apiUrl: "/search/contract"
        searchModels: [Search]
        resultModels: [Contract]
        resultTable: resultFields
        searchFields: searchFields
        defaultSort: { fields: [{ model: "Contract", name: "ctime" }], order: "desc" }

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

  template: tpl
  constructor: (viewName, {sub: subprogram, id: id}) ->
    $.getJSON "/cfg/model/Contract?sid=#{subprogram}&showform=showtable", (mdl) ->
      # Open a contract by its id. If id is null, setup an empty
      # contract form.
      openContract = (cid) ->
        sid = searchVM.subprogram()
        $('a[href="#contract-tab"]').tab("show")
        contractModel = "Contract?sid=#{sid}"
        if cid?
          $('#render-contract').attr(
            "href",
            "/renderContract?contract=#{cid}")
          main.modelSetup(contractModel) contractForm, {id: cid}, {}
        else
          main.modelSetup(contractModel) contractForm,
            # TODO Set committer on server
            {subprogram: sid, committer: parseInt global.user.meta.mid}, {}

        # Role-specific permissions
        kvm = global.viewsWare[contractForm].knockVM
        kvm['isActiveDisableDixi'](true)
        if _.find(global.user.roles, (r) -> r == global.idents("Role").partner)
          kvm['commentDisableDixi'](true)  if kvm['commentDisabled']
        if _.find(global.user.roles, (r) -> r == global.idents("Role").contract_admin)
          kvm['disableDixi'](true)

      spgms = u.newComputedDict "portalSubPrograms"
      def_pgm = spgms.source[0]?.value

      # Search form setup and interaction
      searchVM = screenConstructor mdl

      # Current contract id
      contract = ko.observable null

      # Hide subprogram predicate remove button, make subprogram label bold
      $(".icon-remove").first().parents("div").first().css("visibility","hidden")
      $(".control-label label").first().css("font-weight", "bold")

      # Setup screen from URL
      if subprogram?
        s = parseInt subprogram
        if _.isNumber s
          searchVM.subprogram s
          if id?
            i = parseInt id
            if _.isNumber i
              contract i

      # Update URL&info when subprogram changes
      searchVM.subprogram.subscribe (s) ->
        # Default if the subprogram is erased
        if _.isNull s
          searchVM.subprogram def_pgm
        # Redirect to subprogram URL
        window.location.hash = "contract/#{searchVM.subprogram()}"

      contract.subscribe (c) ->
        sid = searchVM.subprogram()
        if _.isNumber(sid) &&  _.isNumber(c)
          # Redirect to contract URL
          window.location.hash = "contract/#{sid}/#{c}"
          openContract c

      if !searchVM.subprogram()
        # Redirect to default subprogram
        searchVM.subprogram def_pgm
      else
        logoSetup subprogram, spgms.getLab subprogram

      # Open contract from URL
      openContract contract() if contract()

      # Open contracts upon click
      $("#tbl").on "click", "tr", ->
        contract $(this).data("source")

      $("#new-contract-btn").click () ->
        contract null
        openContract contract()
