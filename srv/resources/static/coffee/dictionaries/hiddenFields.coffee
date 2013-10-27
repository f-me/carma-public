define ["dictionaries/local-dict"], (ld) ->
  class HiddenFieldsDict extends ld.dict
    constructor: ({kvm}) ->
      @flds = _.reject kvm._meta.model.fields, (f) -> f.meta?.noadd
      kvm.showField.subscribe (val) =>
        show = ko.utils.unwrapObservable kvm._meta.showFields
        snames = _.pluck show, "name"
        hidden = _.reject @flds, (f) -> _.contains snames, f.name
        @source = _.map hidden, (f) -> { value: f.name, label: f.meta.label }
        @dictValueCache = null
        @dictLabelCache = null
        kvm._meta.showFields(val) if val
        kvm.showField ''
      kvm.showField.valueHasMutated()

    getLab: (val) -> @dictValues()[val]

  dict: HiddenFieldsDict
