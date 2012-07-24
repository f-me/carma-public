this.uploadFile = (e) ->
        form = $(e).parent('form')
        data = form.data()
        url  = form.attr('action')
        fd   = new FormData(form[0])
        xhr  = new XMLHttpRequest()
        # xhr.upload.addEventListener("progress", uploadProgress, false)
        xhr.addEventListener("load", uploadComplete(data), false)
        xhr.addEventListener("error", uploadError, false)
        xhr.addEventListener("abort", uploadError, false)
        xhr.open("POST", url)
        xhr.send(fd)
        form.find('input:file').val("")

uploadComplete = (data) -> (e) ->
  {knockVM, acc} = data
  val = acc()()
  files = JSON.parse e.target.response
  if val
    result = _.union val.split(','), files
    acc() result.join(',')
  else
    acc()(files.join(','))
  knockVM.model().save()

uploadError = (e) ->
  console.log e
  alert "Загрузка завершилась неудачно"

this.deleteFile = (e) ->
  pdata = $(e).parents('ul').data()
  d     = $(e).data()

  deleteCb = (data) ->
    fs = pdata.acc()().split(',')
    fs1 = _.without(fs, data)
    pdata.acc()(fs1.join(','))
    # some strange magic happens here, bb model is changed
    # but not synced, I do not see any reason for that
    pdata.knockVM.model().save()

  return unless confirm "Вы уверены, что хотите удалить #{d.acc()}"

  $.ajax
    'type'     : 'DELETE'
    'url'      : "#{d.knockVM.ctrl}"
    'success'  : deleteCb
    'error'    : (xhr) ->
      if xhr.status == 404
        deleteCb(d.acc())
      else
        alert 'error'
