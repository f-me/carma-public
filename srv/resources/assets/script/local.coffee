#/ Everything local to the customer resides here

{$, _, Mousetrap} = require "carma/vendor"
require "carma/routes"

{BugReport} = require "carma/lib/bug-report"
bugReport = new BugReport

# Some shit depends on it
# (last useful stacktrace path is 'carma/screens/servicesSearch').
window._ = _

# collect errors in console to add them to the bug report
# and then send to server
do ->
  sendError = (err) ->
    unless err.match(/whoopsie/)
      $.ajax
        type: 'POST'
        url : '/whoopsie'
        data: err

  window.onerror = (msg, url, line, pos, err) ->
    bugReport.addError msg, url, line
    sendError err.stack

  # FIXME: report websosket errors also
  $(document).ajaxError (event, jqXHR, s, error) ->
    sendError [
        s.type, s.url, jqXHR.status,
        jqXHR.statusText, s.data, jqXHR.responseText
    ].join('  ')

# This will be called on dom ready.
# We do 'require' here inside because some of modules depends on previous logic.
module.exports.init = ({dicts, user, users}) ->
  $.fn.datepicker.defaults.autoclose = true
  $.fn.datepicker.defaults.enableOnReadonly = false

  bugReport.setElement $('#send-bug-report')

  # Cached mapping between from userid to "name (login)"
  usersById = {}
  users.forEach (i) -> usersById[i.id] =
      id: i.id
      label: "#{i.realName} (#{i.login})"
      roles: i.roles
      isActive: i.isActive
  usersByLabel = {}
  users.forEach (i) -> usersByLabel["#{i.realName} (#{i.login})"] =
      id: i.id
  dicts.users =
    entries:
      for i in users
        id: i.id
        value: String(i.id)
        label: "#{i.realName} (#{i.login})"
        roles: i.roles
        isActive: i.isActive
    byId: usersById
    byLabel: usersByLabel
  dicts.roles =
    entries:
      for i in users
        {value: i.login, label: i.roles}

  main          = require "carma/model/main"
  hooks         = require "carma/hooks/config"
  {LstoreSub}   = require "carma/lstorePubSub"
  neoComponents = require "carma/neoComponents"

  do neoComponents.registerComponents

  # also declares `window.global` crap
  main.setup dicts, hooks, user, new LstoreSub

  window.global.keys = arrows: {left: 37, up: 38, right: 39, down: 40}

  # Doing it here because some of it depends on global messy crappy dungy shit.
  u             = require "carma/utils"
  sync          = require "carma/sync/crud"
  liveMenu      = require "carma/liveMenu"
  CurrentUser   = require "carma/lib/current-user"
  hacking       = require "carma/lib/hacking"
  {CTI}         = require "carma/lib/cti"
  {CTIPanel}    = require "carma/lib/cti-panel"

  do hacking.reenableHacks
  do u.makeAFuckingMess
  do neoComponents.initTopLevelModals

  # disable everytnig websocket-related for portal
  if not window.location.origin.match(/portal\.ruamc\.ru/)
    # Setup CTI panel
    if _.contains user.roles, window.global.idents("Role").cti
      if user.workPhoneSuffix.match(/^\d+$/)
        cti = new CTI user.workPhoneSuffix
        vips = u.newModelDict("VipNumber", false, {dictionaryLabel: 'number'})
        vdns = u.newModelDict("VDN", false, {dictionaryLabel: 'number'})
        opts =
          # AVAYA halts when this is dialed
          bannedNumbers: ["8"]
          displayedToInternal: u.displayedToInternal
          internalToDisplayed: u.internalToDisplayed
          isVipCb: (n) -> vips.getVal(n)
          vdnToDisplayed:
            (vdnNumber) ->
              vdnNumber = vdnNumber?.split(":")[0]
              vdn = vdns.getElement(vdns.getVal(vdnNumber))
              if vdn?
                "#{vdn?.label}: #{vdn?.greeting}"
              else
                null
          onexagentPort: 60000
          # Fill caller phone and program when answering a call on
          # call screen
          answerCallCb: (number, vdnNumber) ->
            if _.contains window.global.user.roles, window.global.idents("Role").call
              number = u.internalToDisplayed number
              if number.length > 5
                vdnNumber = vdnNumber?.split(":")[0]
                vdn = vdns.getElement(vdns.getVal(vdnNumber))
                callData = {}
                if number?
                  callData.callerPhone = number
                else
                  callData.callerPhone = ""
                if vdn?.program
                  callData.program = vdn.program
                u.createNewCall callData
                localStorage["call.search-query"] = "!Тел:" + number
          incomingCallCb: (number, callVM) ->
            $("#cti").show()
            if number.length > 5
              n = encodeURIComponent(u.internalToDisplayed(number))
              $.getJSON "/findContractByPhone/#{n}", (res) ->
                callVM.name(res[0]?.name)

        window.global.CTIPanel = new CTIPanel cti, $("#cti"), opts
        Mousetrap.bind ["`", "ё"], () ->
          $("#cti").toggle()
        Mousetrap.bind "ctrl+enter", () -> window.global.CTIPanel.answer()
      else
        console.error "Malformed workPhoneSuffix \"#{user.workPhoneSuffix}\""

  if user.login == "darya"
    $('#icon-user').removeClass('icon-user').addClass('icon-heart')

  # Enable Popover data API
  $( () -> $('body').popover
                        html: true,
                        selector: '[data-provide="popover"]',
                        trigger: 'hover')

  # disable everytnig websocket-related for portal
  if not window.location.origin.match(/portal\.ruamc\.ru/)
    CurrentUser.initialize()
    window.global.Usermeta.updateAbandonedServices()

  # render menu only after everything else in menu bar is done
  liveMenu.setup(document.getElementById 'nav')

  # file field selection (currenlty only on vin screen)
  $(document).on 'change', '.btn-file :file', ->
    input = $(this)
    numFiles = if input.get(0).files then input.get(0).files.length else 1
    label = input.val().replace(/\\/g, '/').replace(/.*\//, '')
    textInput = $(this).parents('.input-group').find(':text')
    textInput.val(label)
