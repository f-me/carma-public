{_: {pick, memoize}, Immutable: {Record, List}} = require "carma/vendor"


class AvarcomTasksItem extends Record(
  # scalar types
  id        : 0
  label     : ""
  isActive  : false
  isChecked : false
)
  @plainObjScalarProps: ["id", "label", "isActive"]
  @fromPlain: (plainObj) => new @ pick plainObj, @plainObjScalarProps


class AvarcomTasksList extends List
  @Item: AvarcomTasksItem
  @fromPlain: (plainArr) => new @ (@Item.fromPlain x for x in plainArr)
  onlyAvailable: -> @filter (x) -> x.get("isActive")

  constructor: (args...) ->
    list = super args...
    list.onlyAvailable = memoize @onlyAvailable
    return list


module.exports = {
  AvarcomTasksList
  AvarcomTasksItem
}
