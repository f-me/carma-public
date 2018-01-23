{_, ko} = require "carma/vendor"
ld = require "carma/dictionaries/local-dict"

class HiddenFieldsDict extends ld.dict
  constructor: ({kvm}) ->
    @flds  = _.reject kvm._meta.model.fields, (f) -> f.meta?.noadd
    @fldsh = arrToObj 'name', @flds
    kvm.showFields.subscribe (val) =>
      show = _.pluck (ko.utils.unwrapObservable kvm.showFields), "name"
      hidden = _.reject @flds, (f) -> _.contains show, f.name
      @source = _.map hidden, (f) -> { value: f.name, label: f.meta.label }
      @dictValueCache = null
      @dictLabelCache = null

      kvm.fieldsList null

    kvm.fieldsList.subscribe (v) =>
      return unless v
      $_ = _.pluck (ko.utils.unwrapObservable kvm.showFields), "name"
      $_ = $_.concat v
      $_ = _.compact _.map $_, (v) => @fldsh[v]
      kvm.showFields($_)

  getLab: (val) -> @dictValues()[val]

module.exports =
  dict: HiddenFieldsDict
  name: 'HiddenFieldsDict'
