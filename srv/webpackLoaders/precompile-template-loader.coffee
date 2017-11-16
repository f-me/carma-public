_        = require "underscore"
pug      = require "pug-runtime"
{Script} = require "vm"

pugModuleReg        = /(\/|^)pug-runtime(\/index\.js|)$/
includePugModuleReg = /\.pug$/
pugIncludeReg       = /‥pug-include:(.+?)‥/g

pugIncludeReplacer = (match, modulePath) ->
  "\" + require(#{JSON.stringify modulePath}) + \""

pugIncluder = (module) -> "‥pug-include:#{module}‥"

customRequire = (module) ->
  if pugModuleReg.test module
    pug
  else if includePugModuleReg.test module
    pugIncluder.bind null, module
  else
    throw new Error "Unexpected required module: #{module}"

module.exports = (source) ->
  script   = new Script source
  sandbox  = require: customRequire, module: exports: {}
  template = JSON.stringify script.runInNewContext(sandbox) {_}

  withIncludes =
    if pugIncludeReg.test template
      template.replace pugIncludeReg, pugIncludeReplacer
    else
      template

  "module.exports = #{withIncludes};"
