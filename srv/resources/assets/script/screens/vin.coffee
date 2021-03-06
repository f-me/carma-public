{$, _, ko} = require "carma/vendor"

d      = require "carma/dictionaries"
tm     = require "carma/lib/taskmanager"
Main   = require "carma/model/main"
u      = require "carma/utils"
models = require "carma/screens/vin/models"
upl    = require "carma/lib/upload"

template = require "carma-tpl/screens/vin.pug"

# Send VIN file, set up a new box element to track task progress
# TODO heal this jquery spagetti
sendVin = (sid, fid, file, alwaysCb = (->)) ->
  if upl.checkFileSize file
    do alwaysCb
    return

  formData = new FormData
  formData.append "file", file
  formData.append "subprogram", sid
  formData.append "format", fid

  uid = _.uniqueId "vin-"
  box = $ $("#vin-box-template").clone().html()
  box.attr "id", uid
  do box.hide
  box.appendTo $("#vin-box-container")
  do box.fadeIn

  bvm = tm.newTaskVM()
  bvm.filename = file.name
  bvm.bad = ko.observable 0
  bvm.uploaded = ko.observable false
  ko.applyBindings bvm, $(box)[0]

  bvm.errorMsg.subscribe (msg) ->
    box.removeClass "alert-info"
    box.addClass "alert-danger"

  bvm.resultMsg.subscribe (msg) ->
    box.removeClass "alert-info"
    if bvm.fileUrls().length > 0
      box.addClass "alert-warning"
    else
      box.addClass "alert-success"

  resultFmt = (obj) ->
    bvm.bad obj.bad

  upl.ajaxUpload "/vin/upload?subprogram=#{sid}&format=#{fid}",
                 file,
                 xhr: upl.progressXHR box.find ".progress"

    .always () ->
      bvm.uploaded true
      do alwaysCb

    .done (res) ->
      tm.handleTaskWith res.token, bvm, resultFmt, _.identity

    .fail (res) ->
      bvm.errorMsg res.responseText
      bvm.done true

setupVinForm = (viewName, args) ->
  kvm = null

  extendKVM =
    vinIsProcessing: ko.observable no
    vinFilesToUpload: ko.observableArray []

    vinSendHandler: ->
      vinFile = @vinFilesToUpload()[0]
      sid = kvm.subprogram()
      fid = kvm.format()
      if sid? and fid? and vinFile?
        @vinIsProcessing yes
        sendVin sid, fid, vinFile, => @vinIsProcessing no
        @vinFilesToUpload []

  kvm = Main.modelSetup("vinUpload", models.VinUpload) "new-form", {}, {
    extendKVM
    manual_save : true
    slotsee     : ["vin-upload-form"
                   "vin-send"]
  }

module.exports = {
  constructor: setupVinForm
  template
}
