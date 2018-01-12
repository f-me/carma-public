{$, _, md5} = require "carma/vendor"

m = require "carma/sync/datamap"

class BugReport
  constructor: (options) ->
    @sp = "%20"
    @br = "%0A"
    @vertBar = "%7C"
    @comma = "%2C"
    @mail_subject = options?.mail_subject ? ""
    @mail_to = options?.mail_to ? "support@formalmethods.ru"
    @mail_cc = options?.mail_cc ? ""
    @stack = []

  addError: (msg, url, line) =>
    @stack.push "#{msg} #{url} #{line}"

  sendReport: =>
    url = location.href
    models = []

    _.each window.global?.viewsWare, (view) ->
      if q = view.knockVM?._meta?.q
        ftypes = q.ftypes
        model = q.toJSON()
        queue = m.c2sObj q.q, ftypes if q.q
        queueBackup = m.c2sObj q.qbackup, ftypes if q.qbackup
        models.push {model, queue, queueBackup}

    body = "#{@br}#{@br}\
            ========#{@sp}INFORMATION#{@sp}FOR#{@sp}DEVELOPERS#{@sp}========"

    content = {
      url
      console: @stack
      models: models
      user: window.global.user.login
    }

    contentB64 = @_encodeBase64 JSON.stringify content
    body += "#{@br}#{contentB64}#{@vertBar}#{md5 contentB64}"

    location.href = "mailto:#{@mail_to}?cc=#{@mail_cc}\
                     &subject=#{@mail_subject}\
                     &body=#{body}"

    # remove collected errors
    @stack.length = 0

  _encodeBase64: (str) ->
    btoa unescape encodeURIComponent str

  _decodeBase64: (str) ->
    decodeURIComponent escape atob str

module.exports = {bugReport: new BugReport}
