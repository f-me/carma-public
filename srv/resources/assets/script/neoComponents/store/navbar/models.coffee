{_: {pick}, Immutable: {Record, List}} = require "carma/vendor"


class NavbarMenuItem extends Record(
  # scalar types
  type       : ""
  name       : ""
  label      : ""

  # complex types
  screens: null # `null` or `NavbarMenuList`
  nameAliases: null # `null` or list of strings
)
  @plainObjScalarProps: ["type", "name", "label"]
  @fromPlain: (plainObj) =>
    obj = pick plainObj, @plainObjScalarProps
    new @ Object.assign obj,

      screens: if plainObj.screens? \
                  then NavbarMenuList.fromPlain plainObj.screens
                  else null

      nameAliases: if plainObj.nameAliases? \
                   then List().concat plainObj.nameAliases
                   else null


class NavbarMenuList extends List
  @Item: NavbarMenuItem
  @fromPlain: (plainArr) => new @ (@Item.fromPlain x for x in plainArr)

  constructor: (args...) ->
    list = super args...
    return list


module.exports = {
  NavbarMenuList
  NavbarMenuItem
}
