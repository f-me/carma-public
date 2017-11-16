{$, _, ko} = require "carma/vendor"

d      = require "carma/dictionaries"
tm     = require "carma/lib/taskmanager"
Main   = require "carma/model/main"
u      = require "carma/utils"
models = require "carma/screens/vin/models"
upl    = require "carma/lib/upload"

template = require "carma-tpl/screens/vin.pug"

# Send VIN file, set up a new box element to track task progress
sendVin = (sid, fid, file) ->
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
    box.addClass("alert-danger")

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

setupVinForm = (viewName, args) ->
  # vin_html = $el("vin-form-template").html()
  # bulk_partner_html = $el("partner-form-template").html()

  # # Do not show partner bulk upload form when the screen is accessed
  # # by portal users, use appropriate set of programs.
  # all_html = ""

  # if _.contains window.global.user.roles, window.global.idents("Role").psaanalyst
  #   all_html += bulk_partner_html

  # if _.contains window.global.user.roles, window.global.idents("Role").vinAdmin
  #   all_html += vin_html

  # $el(viewName).html(all_html)

  # Program/format selection
  options =
    permEl: null,
    manual_save: true
  kvm = Main.modelSetup("vinUpload", models.VinUpload) "new-form", {}, options
  $("#vin-send").click (e) ->
    vinFile = $("#vin-upload-file")[0].files[0]
    sid = kvm.subprogram()
    fid = kvm.format()
    if sid? && fid? && vinFile?
      sendVin sid, fid, vinFile
      $("#vin-upload-file")[0].value = null
      $("#vin-upload-file").change()
    false

module.exports =
  { constructor: setupVinForm
  , template
  }
