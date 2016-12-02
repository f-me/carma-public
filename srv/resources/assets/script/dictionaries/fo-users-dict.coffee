# Active users which are valid targets for redirecting screen
define ["dictionaries/local-dict"], (ld) ->
  class FoUsersDict extends ld.dict
    constructor: (@opts) ->
      @opts['dict'] = 'users'
      super

    find: (q, cb) ->
      $.bgetJSON "/foUsers", (users) =>
        @source = for u in users
          { value: u[2], label: "#{u[0]} (#{u[1]})" }
      super

  dict: FoUsersDict
