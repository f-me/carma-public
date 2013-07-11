define [ "utils"
       , "hotkeys"
       , "text!tpl/screens/case.html"
       , "model/utils"
       , "model/main"
       ],
  (utils, hotkeys, tpl, mu, main) ->
    utils.build_global_fn 'pickPartnerBlip', ['map']

    # Case view (renders to #left, #center and #right as well)
    setupCaseMain = (viewName, args) -> setupCaseModel viewName, args

    setupCaseModel = (viewName, args) ->

      kvm = main.modelSetup("case") viewName, args,
                         permEl       : "case-permissions"
                         focusClass   : "focusable"
                         slotsee      : ["case-number"]
                         groupsForest : "center"

      ctx = {fields: (f for f in kvm._meta.model.fields when f.meta?.required)}
      setCommentsHandler()

      $("#empty-fields-placeholder").html(
          Mustache.render($("#empty-fields-template").html(), ctx))

      ko.applyBindings(kvm, el("empty-fields"))


      # Render service picker
      #
      # We use Bootstrap's glyphs if "icon" key is set in dictionary
      # entry.
      $("#service-picker-container").html(
        Mustache.render($("#service-picker-template").html(),
                        {dictionary: global.dictionaries["Services"]}))

      $("body").on("change.input", ".redirectOnChange", () ->
          setTimeout(( -> window.location.hash = "back"), 500))

      utils.mkDataTable $('#call-searchtable')
      hotkeys.setup()
      kvm = global.viewsWare[viewName].knockVM

      do (kvm) ->
        ko.computed ->
          nots = (i for i of kvm when /.*Not$/.test i)
          if (_.any nots, (e) -> kvm[e]())
            k["resultDisabled"](true)  for k in kvm["actionsReference"]()
          else
            k["resultDisabled"](false) for k in kvm["actionsReference"]()

    setCommentsHandler = ->
      $("#case-comments-b").on 'click', ->
        i = $("#case-comments-i")
        return if _.isEmpty i.val()
        comment =
          date: (new Date()).toString('dd.MM.yyyy HH:mm')
          user: global.user.login
          comment: i.val()
        k = global.viewsWare['case-form'].knockVM
        if _.isEmpty k['comments']()
          k['comments'] [comment]
        else
          k['comments'] k['comments']().concat comment
        i.val("")


    # Top-level wrapper for storeService
    addService = (name) ->
      kvm = global.viewsWare["case-form"].knockVM
      mu.addReference kvm,
                   'services',
                   { modelName : name },
                   (k) ->
                      e = $('#' + k['view'])
                      e.parent().prev()[0].scrollIntoView()
                      e.find('input')[0].focus()

    utils.build_global_fn 'addService', ['screens/case']

    makeCase = () ->
      v = global.viewsWare['call-form'].knockVM

      callerType = v['callerType']()
      if callerType == 'client' or callerType == 'partner'
        v['callType']('newCase')
      else if not callerType
        v['callerType']('client')
        v['callType']('newCase')

      args =
        contact_name:   v['callerName_name']()
        contact_phone1: v['callerName_phone1']()
        contact_phone2: v['callerName_phone2']()
        contact_phone3: v['callerName_phone3']()
        contact_phone4: v['callerName_phone4']()
        contact_email:  v['callerName_email']()
        contact_contactOwner: v['callerName_contactOwner']()
        contact_ownerName:    v['callerName_ownerName']()
        contact_ownerPhone1:  v['callerName_ownerPhone1']()
        contact_ownerPhone2:  v['callerName_ownerPhone2']()
        contact_ownerPhone3:  v['callerName_ownerPhone3']()
        contact_ownerPhone4:  v['callerName_ownerPhone4']()
        contact_ownerEmail:   v['callerName_ownerEmail']()
        program:        v['program']()
        city:           v['city']()
        car_make:       v['carMake']()
        car_model:      v['carModel']()
        comment:        v['wazzup']()
        callTaker: global.user.meta.realName
      main.buildNewModel 'case', args, {},
        (m, k) ->
          global.router.navigate("case/#{k.id()}", { trigger: true })



    removeCaseMain = ->
      $("body").off "change.input"
      $('.navbar').css "-webkit-transform", ""


    # get partners and show them in table
    # this is called from local.coffe:showCase
    initPartnerTables = ($view,parentView) ->
      m = $view[0].id.match(/(\w*)_partner-view/)
      partnerType = m[1]
      table = $view.find("table##{partnerType}_partnerTable")
      kase = global.viewsWare["case-form"].knockVM
      svc = utils.findCaseOrReferenceVM(parentView)
      # Hide priority columns for when displaying a dealer table
      tblOpts = if partnerType is 'contractor'
        table.css("word-break", "break-all")
        {
          aoColumnDefs: [
            {bVisible: false, aTargets: [2]}
            {sWidth: "50px", aTargets: [5]}
            {sWidth: "10px", aTargets: [6,7,8]}
          ]
        }
      else
        { aoColumns: utils
          .repeat(6, null)
          .concat(utils.repeat(3, { bVisible: false}))
        }

      unless table.hasClass("dataTable")
        utils.mkDataTable table, $.extend(tblOpts, {sScrollY: "200px"})

        fnFormatDetails = (tr) ->
          rowData = dtable.fnGetData tr
          # rowData[9] is partner id, a key in cache attribute
          "<i>" + dtable.data("cache")[rowData[9]].comment + "</i>"

        # Select partner and copy its data to case fields
        table.on "click.datatable", "tr", ->
          name = this.children[1].innerText

          # Highlight the clicked row
          utils.highlightDataTableRow $(this)

          if partnerType is "contractor"
            addr = this.children[2].innerText
            svc["#{partnerType}_address"](addr)
          else
            city = this.children[2].innerText
            addr = this.children[3].innerText
            svc["#{partnerType}_address"]("#{city}, #{addr}")
          svc["#{partnerType}_partner"](name)
          svc["#{partnerType}_partnerId"]($(this).attr('partnerid'))
          # Flush all maps when selecting a partner so that partner
          # icon gets highlighted
          $(".osMap").each (e) ->
            if $(this).hasClass("olMap")
              $(this).data("osmap").events.triggerEvent "moveend"

        # Toggle a comment row
        table.on "click.comment", ".rowexpand", (e) ->
          tr = $(this).parents("tr")[0]
          if dtable.fnIsOpen tr
            dtable.fnClose tr
            $(this).addClass "icon-plus-sign"
            $(this).removeClass "icon-minus-sign"
          else
            dtable.fnOpen tr, (fnFormatDetails tr), "details"
            $(this).addClass "icon-minus-sign"
            $(this).removeClass "icon-plus-sign"
          # Do not select partner when expanding comments
          e.stopPropagation()

      dtable = table.dataTable()

      # hope that contractor_partner is the only partner
      dealer = if partnerType is "contractor" then 0 else 1
      select = ["isActive=1", "isDealer=#{dealer}"]
      select.push("city=#{kase.city()}") if kase.city()
      select.push("makes=#{kase.car_make()}")  if kase.car_make()
      url    = if partnerType is "contractor"
                  "/partnersFor/#{svc._meta.model.name}?#{select.join('&')}"
               else
                  "/allPartners?#{select.join('&')}"
      dict = global.dictValueCache['DealerCities']
      $.getJSON url, (objs) ->
        # Store partner cache for use with maps
        cache = {}
        rows = for p in objs
          p.name = p.name.trim()
          cache[p.id] = p
          ['<i class="rowexpand icon-plus-sign" />',
           p.name        || '',
           dict[p.city]  || '',
           p.addrDeFacto || '',
           p.phone1      || '',
           p.workingTime || '',
           p.priority2   || '',
           p.priority3   || '',
           p.priority1   || '',
           p.id]
        # this last id will never be shown, but I need this, to add
        # partnerid as attribute of the row to pass it then to
        # the service kvm
        dtable.data("cache", cache)
        dtable.fnClearTable()
        dtable.fnSort [[5, "asc"]]
        r = dtable.fnAddData(rows)
        n = dtable.fnSettings().aoData[ r[0] ]
        # this will set partnerid attribute to each row
        # FIXME: find better way to do this
        for i in r
          s  = dtable.fnSettings().aoData[ i ]
          tr = s.nTr
          id = s._aData[9]
          $(tr).attr('partnerid', "partner:#{id}")
          # Highlight the previously selected partner
          if svc["#{partnerType}_partnerId"]() == "partner:#{id}"
            $(tr).addClass("selected-row")

    { constructor       : setupCaseMain
    , destructor        : removeCaseMain
    , template          : tpl
    , addService        : addService
    , makeCase          : makeCase
    , initPartnerTables : initPartnerTables
    }
