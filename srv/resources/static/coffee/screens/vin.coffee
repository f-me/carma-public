define [ "text!tpl/screens/vin.html"
       , "utils"
       , "dictionaries"
       ], (tpl, u, d) ->
  this.setupVinForm = (viewName, args) ->
    vin_html = $el("vin-form-template").html()
    bulk_partner_html = $el("partner-form-template").html()

    # Do not show partner bulk upload form when the screen is accessed
    # by portal users, use appropriate set of programs.
    all_html = ""

    if _.contains global.user.roles, global.idents("Role").psaanalyst
      all_html += bulk_partner_html

    if _.contains global.user.roles, global.idents("Role").vinAdmin
      all_html += vin_html

    $el(viewName).html(all_html)

    # Program/format selection
    dict = (n) -> new d.dicts["ComputedDict"]({ dict: n })
    subprograms = dict('vinPrograms').source
    ko.applyBindings(subprograms, el("vin-subprogram-select"))
    formats = dict('vinFormats').source
    ko.applyBindings(formats, el("vin-format-select"))

    global.viewsWare[viewName] = {}

  this.doVin = ->
    form     = $el("vin-import-form")[0]
    formData = new FormData()
    sid = $("#vin-subprogram-select").val()
    fid = $("#vin-format-select").val()
    vinFile = $("#vin-upload-file")[0].files[0]
    formData.append("file", vinFile)

    uid = _.uniqueId "vin-"
    box = $ $("#vin-box-template").clone().html()
    box.attr "id", uid
    box.hide()
    box.appendTo $("#vin-box-container")
    box.fadeIn()

    bvm =
      filename : ko.observable vinFile.name
      token    : ko.observable null
      status   : ko.observable "загружается"
      report   : ko.observable null
      error    : ko.observable null
    ko.applyBindings bvm, $(box)[0]

    pollToken = (token, bvm) ->
      bvm.token = token
      $.getJSON("/tasks/status/#{token}").
        done((res) ->
          # TODO More stats here
          switch res.status
            when "inprogress"
              bvm.status "обрабатывается"
              setTimeout (() -> pollToken(token, bvm)), 1000
            when "finished"
              box.removeClass "alert-info"
              if res.files?.length > 0
                box.addClass "alert-warning"
                bvm.status "обработан, есть ошибки"
                bvm.report "tasks/getFile/#{token}/#{res.files[0]}"
              else
                box.addClass "alert-success"
                bvm.status "обработан без ошибок"
            when "failed"
              box.removeClass "alert-info"
              box.addClass "alert-error"
              bvm.status "критическая ошибка"
              bvm.error res.msg)


    $.ajax(
      type        : "POST"
      url         : "/vin/upload?subprogram=#{sid}&format=#{fid}"
      data        : formData
      contentType : false
      processData : false
      ).done((res) -> pollToken res.token, bvm)

    return false

  { constructor: setupVinForm
  , template: tpl
  }
