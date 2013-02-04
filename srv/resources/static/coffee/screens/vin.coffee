define [], ->
  this.setupVinForm = (viewName, args) ->
    $el(viewName).html($el("vin-form-template").html())
    global.viewsWare[viewName] = {}

    programs = for v in global.dictionaries.Programs.entries
      p =
        id: v.value
        name: v.label

    ko.applyBindings(programs, el("vin-program-select"))

    setInterval(getVinAlerts, 1000)

  getVinAlerts = ->
    $.getJSON("/vin/state", null, (data) ->
      $("#vin-alert-container").html(
        Mustache.render($("#vin-alert-template").html(), data)))

  this.doVin = ->
    form     = $el("vin-import-form")[0]
    formData = new FormData(form)

    $.ajax(
      type        : "POST"
      url         : "/vin/upload"
      data        : formData
      contentType : false
      processData : false
      ).done((msg)-> alert( "Result: " + msg))

  this.removeVinAlert = (val) -> $.post "/vin/state", { id: val }

  { constructor: setupVinForm }