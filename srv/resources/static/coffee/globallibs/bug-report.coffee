define ["sync/datamap"], (m) ->

  class BugReport
    constructor: (options) ->
      @sp = "%20"
      @br = "%0A"
      @setElement options.el if options.el
      @mail_subject = options.mail_subject or
                      "BUG:#{@sp}Сообщение#{@sp}из#{@sp}CaRMa"
      @mail_to = options.mail_to or "support@formalmethods.ru"
      @mail_cc = options.mail_cc or "pavel.golovnin@ruamc.ru"
      @stack = []

      @error = console.error
      console.error = =>
        args = Array.prototype.slice.call arguments
        @stack.push args
        @error.apply console, args

    setElement: (el) =>
      @$element = $(el)
      @$element.on('click', @sendReport)

    sendReport: =>
      url = location.href
      model = []
      queue = []
      queueBackup = []

      _.each global.viewsWare, (view) ->
        if view.knockVM
          model.push view.knockVM._meta.q.toJSON()
          ftypes = {}
          ftypes[f.name] = f.type for f in view.model.fields
          queue.push JSON.stringify m.c2sObj(view.knockVM._meta.q.q, ftypes)
          queueBackup.push JSON.stringify m.c2sObj(view.knockVM._meta.q.qbackup, ftypes)

      body = "#{@br}Ваша последовательность действий, при которой воспроизводится ошибка. Опишите пожалуйста максимально подробно."
      body += "#{@br}"
      body += "#{@br}Какой результат вы ожидали получить? По возможности обоснуйте, почему именно этого результата Вы ожидали."
      body += "#{@br}"
      body += "#{@br}Какой результат был получен?"
      body += "#{@br}"
      body += "#{@br}"

      body += "#{@br}======== Информация для разработчиков ========"
      body += "#{@br}URL:#{url}"

      consoleBase64 = @encodeBase64 @stack.join(@br)
      body += "#{@br}Console:#{consoleBase64}"
      body += "#{@br}Console MD5:#{md5 consoleBase64}"

      modelBase64 = @encodeBase64 model.join(@br)
      body += "#{@br}Model:#{modelBase64}"
      body += "#{@br}Model MD5:#{md5 modelBase64}"

      queueBase64 = @encodeBase64 queue.join(@br)
      body += "#{@br}Queue:#{queueBase64}"
      body += "#{@br}Queue MD5:#{md5 queueBase64}"

      queueBackupBase64 = @encodeBase64 queueBackup.join(@br)
      body += "#{@br}Queue Backup:#{queueBackupBase64}"
      body += "#{@br}Queue Backup MD5:#{md5 queueBackupBase64}"

      location.href = "mailto:#{@mail_to}?cc=#{@mail_cc}
                       &subject=#{@mail_subject}
                       &body=#{body}"

    encodeBase64: (str) ->
      btoa unescape encodeURIComponent str

    decodeBase64: (str) ->
      decodeURIComponent escape atob str

  BugReport: BugReport

