define [ "text!tpl/screens/vin.html"
       , "dictionaries"
       , "lib/taskmanager"
       , "utils"
       , "screens/uploads"
       ], (tpl, d, tm, u, upl) ->
  @setupVinForm = (viewName, args) ->
    # vin_html = $el("vin-form-template").html()
    # bulk_partner_html = $el("partner-form-template").html()

    # # Do not show partner bulk upload form when the screen is accessed
    # # by portal users, use appropriate set of programs.
    # all_html = ""

    # if _.contains global.user.roles, global.idents("Role").psaanalyst
    #   all_html += bulk_partner_html

    # if _.contains global.user.roles, global.idents("Role").vinAdmin
    #   all_html += vin_html

    # $el(viewName).html(all_html)

    # Program/format selection
    subprograms = u.newComputedDict("vinPrograms").source
    ko.applyBindings subprograms, el("vin-subprogram-select")
    formats = u.newModelDict("VinFormat").source
    ko.applyBindings formats, el("vin-format-select")

    $("#vin-send").click (e) ->
      vinFile = $("#vin-upload-file")[0].files[0]
      sid = $("#vin-subprogram-select").val()
      fid = $("#vin-format-select").val()
      if sid? && fid? && vinFile?
        sendVin sid, fid, vinFile
      false

  # Send VIN file, set up a new box element to track task progress
  @sendVin = (sid, fid, file) ->
    if upl.checkFileSize(file)
      return
    formData = new FormData()
    formData.append("file", file)
    formData.append("subprogram", sid)
    formData.append("format", fid)

    uid = _.uniqueId "vin-"
    box = $ $("#vin-box-template").clone().html()
    box.attr "id", uid
    box.hide()
    box.appendTo $("#vin-box-container")
    box.fadeIn()

    bvm = tm.newTaskVM()
    bvm.filename = file.name
    bvm.bad = ko.observable 0
    bvm.uploaded = ko.observable false
    ko.applyBindings bvm, $(box)[0]

    bvm.errorMsg.subscribe (msg) ->
      box.removeClass("alert-info")
      box.addClass("alert-error")

    bvm.resultMsg.subscribe (msg) ->
      box.removeClass("alert-info")
      if bvm.fileUrls().length > 0
        box.addClass("alert-warning")
      else
        box.addClass("alert-success")

    resultFmt = (obj) ->
      bvm.bad obj.bad

    upl.ajaxUpload("/vin/upload?subprogram=#{sid}&format=#{fid}", file,
      xhr: upl.progressXHR box.find ".progress"
      ).
      always(() -> bvm.uploaded true).
      done((res) ->
        tm.handleTaskWith res.token, bvm, resultFmt, _.identity).
      fail((res) ->
        bvm.errorMsg res.responseText
        bvm.done true)

  { constructor: setupVinForm
  , template: tpl
  }
