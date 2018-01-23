# Asynchronous file upload helpers, inline-uploader widget handlers.

{$, _} = require "carma/vendor"

# Produce an XHR-creating callback which updates a Bootstrap
# .progress element as the file upload progresses (use in xhr
# argument $.ajax).
progressXHR = (progress) -> () ->
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
checkFileSize = (file) ->
  max = window.global.config("max-file-size")
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
ajaxUpload = (url, file, props) ->
  data = new FormData
  data.append "file", file

  baseProps = {
    url
    data
    type        : "POST"
    dataType    : "json"
    contentType : false
    processData : false
  }

  $.ajax Object.assign baseProps, props

module.exports = {
  ajaxUpload
  progressXHR
  checkFileSize
}
