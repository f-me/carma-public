# Asynchronous file upload helpers, inline-uploader widget handlers.

{$, _} = require "carma/vendor"

@ = {}

# Produce an XHR-creating callback which updates a Bootstrap
# .progress element as the file upload progresses (use in xhr
# argument $.ajax).
@progressXHR = (progress) -> () ->
  hideProgress = () -> progress.hide()
  hideProgress()
  xhr = new XMLHttpRequest()
  xhr.upload.addEventListener("progress",
    (e) ->
      progress.show()
      if e.lengthComputable
        fraction = e.loaded / e.total
        percent = fraction * 100.0
        progress.find(".progress-bar").css "width", percent + "%"
    , false)
  xhr.upload.addEventListener "load", hideProgress, false
  xhr.upload.addEventListener "error", hideProgress, false
  return xhr

# Return true if a file is oversized
@checkFileSize = (file) ->
  max = global.config("max-file-size")
  maxMb = max / (1024.0 * 1024.0)
  if file.size > max
    window.alert "Размер файла #{file.name}
      превышает допустимый (#{maxMb} Мб)!
      Файл не будет загружен"
    true
  else
    false

# Base object for asynchronous upload requests
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

# Add an attachment reference to an instance, provided a form from
# inline-uploader template
@inlineUploadFile = (form) ->
  formMeta = form.data()
  url      = form.attr('action')
  files    = form.find(".upload-dialog")[0].files
  if files.length > 0
    @ajaxUpload(url, files[0]).
    fail((e) ->
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
  ref = "Attachment:#{attId}"

  return unless confirm "Вы уверены, что хотите открепить этот файл?"

  formMeta = e.parent().parent().siblings("form").data()
  kvm = formMeta.knockVM

  # Cut out attachment ref and re-save the instance
  kvm[field] _.without(kvm[field]().split(','), ref).join(',')
  kvm._meta.q.save()

  # Suitable for onClick on <a>
  false

module.exports = {
  inlineUploadFile
  inlineDetachFile
  ajaxUpload
  progressXHR
  checkFileSize
}
