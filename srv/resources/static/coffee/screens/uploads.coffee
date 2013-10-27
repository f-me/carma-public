# Bulk uploads screen handlers, inline-uploader widget handlers.
#
# Uses "attachment" model and /upload/case/[bulk|<n>]/files server
# handlers.
define [ "text!tpl/screens/uploads.html"
       , "model/utils"], (tpl, mu) ->
  # Base object for asynchronous requests to /uploads.
  #
  # props is an object with extra properties of $.ajax
  @ajaxUpload = (url, file, props) ->
    fd = new FormData()
    fd.append("file", file)

    baseProps = 
      type        : "POST"
      url         : url
      contentType : false
      processData : false
      data        : fd
      dataType    : "json"
    
    $.ajax _.extend(baseProps, props)
  
  # Destructively add a reference to attachment:<attId> to files field
  # of a case instance object
  #
  # Return true if a reference to this attachment is already present
  #
  # TODO This might be replaced by a regular reference-adding routine
  @addAttIdToCaseObj = (attId, caseObj) ->
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

  # Remove an attachment reference from case, provided file's bvm and
  # case number.
  @detachFromCase = (bvm, caseId) ->
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
  @attachToCase = (bvm, af) ->
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

  # Upload new attachment, render file progress bar and whistles.
  # Argument is a File object.
  @sendFile = (file) ->
    uid = _.uniqueId "upload-"

    # A box for this upload, with a progress bar and whistles
    box = $ $("#upload-box-template").clone().html()
    box.attr "id", uid
    box.hide()
    box.appendTo $("#uploaded-files")
    box.fadeIn()

    # A small KVM for user interaction
    bvm =
      msg      : ko.observable "Выполняю загрузку…"
      filename : ko.observable file.name
      aid      : ko.observable null
      # Array of case ids this file has been attached to
      cases    : ko.observableArray()
      # Array of unknown case ids from filename
      unknown  : ko.observableArray()
      # True if the file was uploaded as a duplicate
      dupe     : ko.observable false
      # Allow attaching to extra cases
      attach : ko.observable false
    ko.applyBindings bvm, $(box)[0]

    # Upload the file asynchronously
    @ajaxUpload("/upload/case/bulk/files/", file,
      xhr: () ->
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
      always(() ->
        box.find(".progress").fadeOut()
        box.removeClass "alert-info").
      fail(() ->
        box.addClass "alert-error"
        bvm.msg "Во время загрузки произошла критическая ошибка").
      done((res) ->
        bvm.aid(res.attachment.id)
        bvm.filename(res.attachment.filename)
        bvm.dupe(res.dupe)
        
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
        box.on "click", ".detach-button", () ->
          detachFromCase bvm, $(this).data("target")
          false

        # Hook up case add routine for input field
        box.find(".attach-form").submit (e) ->
          af = $(this).find(".attach-field")
          res = attachToCase bvm, af
          false

        # Forget existing validity violations when case number changes
        box.find(".attach-field").change () ->
          $(this)[0].setCustomValidity("");

        box.data("bvm", bvm)
        )

  # Render file browser widget, setup all hooks and screen-global
  # handlers
  @renderUploadsForm = (viewName, args) ->
    $("#upload-files-tip").tooltip()
    $("#upload-cleanup-tip").tooltip()

    # Fake browse button
    $("#upload-browse-btn").click () ->
      $("#upload-dialog").click()

    $("#upload-cleanup").click () ->
      $("#uploaded-files .alert-success").slideUp()

    $("#upload-files-form").submit (e) ->
      $("#upload-send").click()
      false

    # Show file names when selecting files
    $("#upload-dialog").change () ->
      files = @files
      if files.length > 0
        names = (files.item(n).name for n in [0..(files.length - 1)])
        $("#upload-names").val names.join " "
      else
        $("#upload-names").val ""

    $("#upload-send").click () ->
      # Upload all files
      files = $("#upload-dialog")[0].files
      if files.length > 0
        for n in [0..(files.length - 1)]
          sendFile files.item(n)
        # Flush files list
        $("#upload-dialog").val("")
        $("#upload-dialog").change()

  # Add an attachment reference to an instance, provided a form from
  # inline-uploader template
  @inlineUploadFile = (form) ->
    formMeta = form.data()
    url      = form.attr('action')
    files    = form.find(".upload-dialog")[0].files
    if files.length > 0
      @ajaxUpload(url, files[0]).
      fail((e) ->
        console.log e
        alert "Не удалось загрузить файл!"
      ).
      # Re-read instance data when a new attachment is added
      done(() -> formMeta.knockVM._meta.q.fetch())
      
      form.find('input:file').val("").trigger("change")

  # Delete an attachment reference from an instance, provided an
  # element from reference template with data-attachment and
  # data-field fields set.
  @inlineDetachFile = (e) ->
    attId = e.data("attachment")
    field = e.data("field")
    ref = "attachment:#{attId}"

    return unless confirm "Вы уверены, что хотите открепить этот файл?"
    
    formMeta = e.parent().parent().siblings("form").data()
    kvm = formMeta.knockVM

    # Cut out attachment ref and re-save the instance
    kvm[field] _.without(kvm[field]().split(','), ref).join(',')
    kvm._meta.q.save()

    # Suitable for onClick on <a>
    false
    
  { constructor:      renderUploadsForm
  , inlineUploadFile: inlineUploadFile
  , inlineDetachFile: inlineDetachFile
  , template:         tpl
  }
