{_: {pick, memoize}, Immutable: {Record, List}} = require "carma/vendor"


class SmsTemplateItem extends Record(
  # scalar types
  isActive : false
  label    : ""
  text     : ""
  id       : 0
)
  @plainObjScalarProps: ["isActive", "label", "text", "id"]
  @fromPlain: (plainObj) => new @ pick plainObj, @plainObjScalarProps


class SmsTemplateList extends List
  @Item: SmsTemplateItem
  @fromPlain: (plainArr) => new @ plainArr.map (x) => @Item.fromPlain x
  onlyActive: -> @filter (x) -> x.get "isActive"

  constructor: (args...) ->
    list = super args...
    list.onlyActive = memoize @onlyActive
    return list


module.exports = {
  SmsTemplateList
  SmsTemplateItem
}
