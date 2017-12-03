{_: {pick, memoize}, Immutable: {Record, List}} = require "carma/vendor"


class SlideItem extends Record(
  # scalar types
  id: 0
  isRoot: false

  # complex types
  # …
)
  @plainObjScalarProps: ["id", "isRoot"]
  @fromPlain: (plainObj) => new @ pick plainObj, @plainObjScalarProps


module.exports = {
  SlideItem
}
