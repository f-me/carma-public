# WARNING! This module will be precompiled.
# See for details: srv/webpackLoaders/precompile-code-loader.coffee

importanceColorNameAliases = Object.freeze
  violet : "#99f"
  red    : "#f66"
  yellow : "#fc3"
  orange : "#f60"
  green  : "#9f6"

importanceColorsR = Object.freeze [
  "violet"
  "red"
  "yellow"
  "orange"
  "green"
].reverse()

importanceHexColors =
  Object.freeze (importanceColorNameAliases[x] for x in importanceColorsR)

module.exports = {importanceHexColors}
