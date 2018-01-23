# Active users which are valid targets for manual action assignment
# from supervisor screen
{$} = require "carma/vendor"
ld = require "carma/dictionaries/local-dict"

class BoUsersDict extends ld.dict
  constructor: (@opts) ->
    @opts['dict'] = 'users'
    super

  find: (q, cb) ->
    $.bgetJSON "/boUsers", (users) =>
      @source = for u in users
        { value: u[2], label: "#{u[0]} (#{u[1]})" }
    super

module.exports =
  dict: BoUsersDict
  name: 'BoUsersDict'
