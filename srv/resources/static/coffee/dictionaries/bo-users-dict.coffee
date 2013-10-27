define ["dictionaries/local-dict"], (ld) ->
  class BoUsersDict extends ld.dict
    constructor: (@opts) ->
      @opts['dict'] = 'users'
      super

    find: (q, cb) ->
      console.log 'bofind', @, @source
      $.bgetJSON "/boUsers", (users) =>
        @source = for u in users
          { value: u.login, label: "#{u.name} (#{u.login})" }
      super

  dict: BoUsersDict
