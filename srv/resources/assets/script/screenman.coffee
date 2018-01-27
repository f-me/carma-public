{Table} = require "carma/tableman"

class Screen
  constructor: (@callback) ->
    @table = null

  show: ->
    do @callback
    do @table?.show

  addTable: (params) ->
    @table = new Table params

  getTable: ->
    @table

class ScreenMan
  # screens array
  # key -> screen's name
  # value -> instance of the Screen class
  screens = []

  addScreen: (name, callback) ->
    screens[name] = new Screen callback

  showScreen: (name) ->
    do screens[name]?.show

module.exports.screenMan = new ScreenMan
