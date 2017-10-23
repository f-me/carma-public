# Active users which are valid targets for redirecting screen
{$} = require "carma/vendor"
ld = require "carma/dictionaries/local-dict"

class LoggedUsersDict extends ld.dict
  constructor: (@opts) ->
    @opts['dict'] = 'users'
    super

  find: (q, cb) ->
    $.bgetJSON "/loggedUsers?#{@opts.meta?.filterBy}", (users) =>
      @source = for u in users
        { value: u[2], label: "#{u[0]} (#{u[1]})" }
    super

module.exports =
  dict: LoggedUsersDict
  name: 'LoggedUsersDict'
