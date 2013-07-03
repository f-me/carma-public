# Bulk uploads screen
#
# Uses "attachment" model and /upload/case/bulk/files handler.
define [ "text!tpl/screens/uploads.html"
       , "model/utils"], (tpl, mu) ->
  # Destructively add a reference to attachment:<attId> to files field
  # of case instance object
  #
  # Return true if a reference to this attachment is already present
  #
  # TODO This might be replaced by a regular reference-adding routine
  this.addAttIdToCaseObj = (attId, caseObj) ->
    ref = "attachment:" + attId
    if caseObj.files? && caseObj.files.length > 0
      # No dupe references
      if caseObj.files.search(ref) == -1
        caseObj.files = caseObj.files + "," + ref
        return true
      else
        return false
    else
      caseObj.files = ref
      return true

  # Remove an attachment reference from case, provided an element with
  # associated data-target="<caseId>" attribute within an .upload-box
  # element.
  this.detachFromCase = (link) ->
    bvm = $(link).parents(".upload-box").data("bvm")
    caseId = $(link).data("target")
    caseUrl = "/_/case/" + caseId
    attId = bvm.aid()
    ref = "attachment:" + attId

    $.getJSON(caseUrl).
      done((res) ->
        newFiles = _.without(res.files.split(","), ref).join(",")
        $.putJSON(caseUrl, {files: newFiles}).
          done(() -> bvm.cases.remove caseId.toString()))

  # Attach a file to a case, provided file's bvm and a field
  # containing case number. Check for dupes/unknown cases. Set af's
  # validity in case of error. Push new case id to bvm.cases.
  this.attachToCase = (bvm, af) ->
    caseId = af.val()
    caseUrl = "/_/case/" + caseId
    attId = bvm.aid()

    if caseId.length == 0
      af[0].setCustomValidity "Не задан номер кейса"
      return

    $.getJSON(caseUrl).
      fail(() ->
        af[0].setCustomValidity "Кейс не найден"
      ).
      done((res) ->
        # Patch "files" field of the object and send it back to server
        # using regular CRUD interface
        notDupe = addAttIdToCaseObj attId, res
        if !notDupe
          af[0].setCustomValidity "К этому кейсу данный файл уже прикреплён"
        else
          # Clear input (not gonna fix it)
          af.val("")
          # Store new reference
          $.putJSON(caseUrl, {files: res.files}).
            done(() -> bvm.cases.push caseId))

  # Upload new attachment, render file progress bar and whistles
  this.sendFile = (file) ->
    fd = new FormData()
    fd.append("file", file)

    uid = _.uniqueId "upload-"

    # A box for this upload, with a progress bar and whistles
    box = $ $("#upload-box-template").clone().html()
    box.attr "id", uid
    box.hide()
    box.appendTo $("#uploaded-files")
    box.fadeIn()

    # A small KVM for user interactaction
    bvm =
      msg      : ko.observable "Выполняю загрузку…"
      filename : ko.observable file.name
      aid      : ko.observable null
      cases    : ko.observableArray()
      # Array of unknown case ids from filename
      unknown  : ko.observableArray()
      # Allow attaching to extra cases
      attach : ko.observable false
    ko.applyBindings bvm, $(box)[0]

    # Upload the file asynchronously
    $.ajax(
      type        : "POST"
      url         : "/upload/case/bulk/files/"
      contentType : false
      processData : false
      data        : fd
      dataType    : "json"
      xhr         : () ->
        xhr = new XMLHttpRequest()
        xhr.upload.addEventListener("progress",
          (e) ->
            if e.lengthComputable
              # Update progress bar as upload progresses
              fraction = e.loaded / e.total
              percent = fraction * 100.0
              box.find(".bar").css "width", percent + "%"
          , false)
        return xhr
      ).
      always((res) ->
        box.find(".progress").fadeOut()
        box.removeClass "alert-info").
      fail((res) ->
        box.addClass "alert-error"
        bvm.msg "Во время загрузки произошла критическая ошибка").
      done((res) ->
        bvm.aid(res.attachment.id)
        bvm.filename(res.attachment.filename)
        for t in res.targets
          bvm.cases.push t[1]
        for t in res.unknown
          bvm.unknown.push t[1]
        if bvm.cases().length == 0
          box.addClass "alert-error"
          bvm.msg "Файл загружен, но номера кейсов не распознаны"
        else
          if bvm.unknown().length > 0
            box.addClass "alert-warning"
            bvm.msg "Файл загружен, но некоторые номера кейсов не распознаны"
          else
            box.addClass "alert-success"
            bvm.msg "Файл успешно загружен"

        bvm.attach true

        # Activate "add case" link
        box.find(".attach-button").click () ->
          $(this).hide()
          box.find(".attach-form").show().find(".attach-field").focus()
          false
        box.find(".attach-field").blur () ->
          if $(this).val().length == 0
            $(this).parents(".attach-form").hide()
            box.find(".attach-button").show()

        # "Delete case" links
        box.find(".detach-button").click () ->
          detachFromCase bvm, $(this).data("target")
          bvm.cases.remove $(this).data("target")
          false

        # Hook up case add routine for input field
        box.find(".attach-form").submit (e) ->
          af = $(this).find(".attach-field")
          res = attachToCase bvm, af

        # Forget existing validity violations
        box.find(".attach-field").change () ->
          $(this)[0].setCustomValidity("");

        box.data("bvm", bvm)
        )

  # Render file browser widget, setup all hooks and screen-global
  # handlers
  this.renderUploadsForm = (viewName, args) ->
    $("#upload-files-tip").tooltip()
    $("#upload-cleanup-tip").tooltip()

    # Fake browse button
    $("#upload-browse-btn").click () ->
      $(this).siblings("#upload-dialog").click()

    $("#upload-cleanup").click () ->
      $("#uploaded-files .alert-success").slideUp()

    # Show file names when selecting files
    $("#upload-dialog").change () ->
      files = this.files
      if files.length > 0
        names = (files.item(n).name for n in [0..(files.length - 1)])
        $(this).siblings("#upload-names").val names.join " "
      else
        $(this).siblings("#upload-names").val ""

    $("#upload-send").click () ->
      # Upload all files
      files = $(this).siblings("#upload-dialog")[0].files
      for n in [0..(files.length - 1)]
        sendFile files.item(n)
      # Flush files list
      $("#upload-dialog").val("")
      $("#upload-dialog").change()

  { constructor: renderUploadsForm
  , template: tpl
  }
