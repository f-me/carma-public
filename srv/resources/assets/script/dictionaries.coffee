{_} = require "carma/vendor"

dicts = _.reduce [
  require "carma/dictionaries/local-dict"
  require "carma/dictionaries/addresses-dict"
  require "carma/dictionaries/contracts-dict"
  require "carma/dictionaries/bo-users-dict"
  require "carma/dictionaries/logged-users-dict"
  require "carma/dictionaries/computed-dict"
  require "carma/dictionaries/dealers-dict"
  require "carma/dictionaries/model-dict"
  require "carma/dictionaries/results-dict"
  require "carma/dictionaries/hiddenFields"
  require "carma/dictionaries/consultant-dict"
], ((obj, {name, dict}) -> obj[name] = dict if dict?; obj), {}

module.exports = {
  dicts

  dictFromMeta: (kvm, meta) ->
    type = meta.dictionaryType
    opts =
      kvm   : kvm
      dict  : meta.dictionaryName
      parent: meta.dictionaryParent
      meta  : meta
    new dicts[type || 'LocalDict'](opts)
}
