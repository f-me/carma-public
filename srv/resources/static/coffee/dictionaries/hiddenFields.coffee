define ["dictionaries/local-dict"], (ld) ->
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

        kvm.fieldsList ''

      kvm.fieldsList.subscribe (v) =>
        return unless v
        $_ = _.pluck (ko.utils.unwrapObservable kvm.showFields), "name"
        $_ = $_.concat v
        $_ = _.map $_, (v) => @fldsh[v]
        kvm.showFields($_)

    getLab: (val) -> @dictValues()[val]

  dict: HiddenFieldsDict
