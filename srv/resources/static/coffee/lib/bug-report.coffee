define ["sync/datamap"], (m) ->

  class BugReport
    constructor: (options) ->
      @sp = "%20"
      @br = "%0A"
      @vertBar = "%7C"
      @setElement options.el if options?.el
      @mail_subject = options?.mail_subject or
                      "BUG:#{@sp}Users#{@sp}report#{@sp}from#{@sp}CaRMa"
      @mail_to = options?.mail_to or "support@formalmethods.ru"
      @mail_cc = options?.mail_cc or "pavel.golovnin@ruamc.ru"
      @stack = []

    setElement: (el) =>
      @$element = $(el)
      @$element.on('click', @sendReport)

    addError: (msg, url, line) =>
      @stack.push "#{msg} #{url} #{line}"

    sendReport: =>
      url = location.href
      models = []

      _.each global?.viewsWare, (view) ->
        if q = view.knockVM?._meta?.q
          ftypes = q.ftypes
          model = q.toJSON()
          queue = m.c2sObj(q.q, ftypes) if q.q
          queueBackup = m.c2sObj(q.qbackup, ftypes) if q.qbackup
          models.push {model, queue, queueBackup}

      body  = "#{@br}"
      body += "#{@br}========#{@sp}INFORMATION#{@sp}FOR#{@sp}DEVELOPERS#{@sp}========"
      content =
        url: url,
        console: @stack,
        models: models
        user: global.user.login
      contentB64 = @encodeBase64 JSON.stringify content
      body += "#{@br}#{contentB64}#{@vertBar}#{md5 contentB64}"

      location.href = "mailto:#{@mail_to}?cc=#{@mail_cc}
                       &subject=#{@mail_subject}
                       &body=#{body}"

      # remove collected errors
      @stack.length = 0

    encodeBase64: (str) ->
      btoa unescape encodeURIComponent str

    decodeBase64: (str) ->
      decodeURIComponent escape atob str

  BugReport: BugReport

