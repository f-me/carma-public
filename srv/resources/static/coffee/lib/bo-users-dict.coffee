define ["lib/local-dict", ], (m) ->
  class BoUsersDict extends m.dict
    constructor: (@opts) ->
      @opts['dict'] = 'users'
      super(@opts)
      $.getJSON "/boUsers", (users) =>
        @source = for u in users
          { value: u.login, label: "#{u.name} (#{u.login})" }

  dict: BoUsersDict
