define ["text!tpl/screens/uploads.html"], (tpl) ->
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
      cases    : ko.observableArray()
      # Array of unknown case ids from filename
      unknown  : ko.observableArray()
    ko.applyBindings bvm, $(box).get(0)

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
      done((res) ->
        bvm.filename(res.attachment.filename)
        for t in res.targets
          bvm.cases.push t[1]
        for t in res.unknown
          bvm.unknown.push t[1]
        if bvm.unknown().length > 0
          box.addClass "alert-warning"
          bvm.msg "Файл загружен, но некоторые номера кейсов не распознаны"
        else
          if bvm.cases().length == 0
            box.addClass "alert-error"
            bvm.msg "Файл загружен, но номера кейсов не распознаны"
          else
            box.addClass "alert-success"
            bvm.msg "Файл успешно загружен"
        ).
      fail((res) ->
        box.addClass "alert-error"
        bvm.msg "Во время загрузки произошла критическая ошибка")


  this.renderUploadsForm = (viewName, args) ->
    $("#upload-files-tip").tooltip()

    # Fake browse button
    $("#upload-browse-btn").click () ->
      $(this).siblings("#upload-dialog").click()

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
