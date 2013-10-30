define [ "text!tpl/screens/vin.html"
       , "utils"
       , "lib/ident/role"
       , "dictionaries"
       ], (tpl, u, role, d) ->
  this.setupVinForm = (viewName, args) ->
    vin_html = $el("vin-form-template").html()
    partner_html = $el("partner-form-template").html()

    # Do not show partner bulk upload form when the screen is accessed
    # by portal users, use appropriate set of programs.
    dict = (n) -> new d.dicts["ComputedDict"]({ dict: n })
    programs = dict('vinPrograms').source
    if _.contains(global.user.roles, role.psaanalyst)
      all_html = vin_html
    else
      all_html = vin_html + partner_html


    $el(viewName).html(all_html)

    global.viewsWare[viewName] = {}
    ko.applyBindings(programs, el("vin-program-select"))

    setInterval(getVinAlerts, 1000)

  getVinAlerts = ->
    $.getJSON("/vin/state", null, (data) ->
      $("#vin-alert-container").html(
        Mustache.render($("#vin-alert-template").html(), data)))

  this.doVin = ->
    form     = $el("vin-import-form")[0]
    formData = new FormData()
    pid = $("#vin-program-select").val()
    vinFile = $("#vin-upload-file")[0].files[0]
    formData.append("file", vinFile)

    $.ajax(
      type        : "POST"
      url         : "/vin/upload?program=" + pid
      data        : formData
      contentType : false
      processData : false
      ).done((msg)-> alert( "Result: " + msg))

  removeVinAlert = (val) -> $.post "/vin/state", { id: val }

  u.build_global_fn 'removeVinAlert', ['screens/vin']

  { constructor: setupVinForm
  , template: tpl
  }
