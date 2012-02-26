$(function(){
    window.global = window.global || {};
    window.global.viewPlugin = viewPlugins();
    window.global.fieldTemplate = fieldTemplates();
    window.global.dictionary = dictionaries();
    window.global.meta = {
        page: metaPages(),
        form: metaForms(),
      };
    window.global.viewModel = {};

    var menuRouter = initBottomMenu();
    menuRouter.bind("route:updMenu", function(sect) {
      if (sect === "") {
        var parts = window.location.pathname.split("/");
        sect = parts.length > 1 ? parts[1] : sect;
      }
      renderPage(global.meta.page[sect]); //FIXME: injection //TODO: pass `arguments`
    });

    Backbone.history.start({pushState: true});
    $(".field:first").focus();

    $elem("SelectCase.new").live("click", function(e) {
      e.preventDefault();
      var newcase = {
        wazzup: elem("CallInfo.wazzup").value,
        contactName: elem("GeneralContact.name").value,
        contactPhone: elem("GeneralContact.phone").value};
      ajaxPOST("/api/case", newcase, function(id) {
        menuRouter.navigate("/case/"+id, {trigger:true});
      });
    });
    $elem("CaseInfo.addService").live("click", function(e) {
      e.preventDefault();
      var svc = 
    });
});

function ajaxPOST(url, data, callback) {
  return $.ajax({
    type:"POST",
    url:url,
    contentType: "application/json; charset=utf-8",
    data:JSON.stringify(data),
    processData:false,
    dataType:"json",
    success:callback
  });
}


//FIXME: redirect somewhere when `pageModel` is undefined?
function renderPage(pageModel) {
  // remove all forms from all containers
  $(".column").children().detach();

  _.each(pageModel, function(containerModels, containerId) {
      _.each(containerModels, function(formId) {
        var form = createForm(formId, global.meta.form[formId]);
        $("#"+containerId).append(form);
      });
  });
  ko.applyBindings(global.viewModel);
}


function createForm(formId, formMeta) {
  var form = $("<fieldset/>");
  form.attr("id",formId);
  var vm = {};

  global.viewModel[formId] = vm; 

  if (_.has(formMeta, "title")) {
    form.append("<legend>" + formMeta.title + "</legend>");
  }

  _.each(formMeta.fields, function(f) {
        var field = createField(formId, vm, f);
        field.appendTo(form);
      try {
      } catch(e) {
        console.log(e);
      }
  });
  return form;
}

function createField(formId, vm, f) {
  var field;
  _.each(f, function(fieldMeta, fieldId) {
    //apply defaults to filed description
    fieldMeta = _.defaults(fieldMeta, {
      type: "text",
      id: formId + "." + fieldId,
      default: "",
    });
    if (fieldMeta.data) {
      var data = global.dictionary[fieldMeta.data]; 
      fieldMeta.data = data || fieldMeta.data; 
    }

    //apply field template to field description to create
    //corresponding html element
    field = $(global.fieldTemplate[fieldMeta.type](fieldMeta));
    var realField = field.find(".field");
    realField.attr('id',fieldMeta.id);

    //apply additional plugins
    _.each(fieldMeta,function(val,key) {
        if (_.has(global.viewPlugin, key)) {
          global.viewPlugin[key](realField,fieldMeta);
        }
    });

    vm[fieldId] = ko.observable(fieldMeta.default);
  });
  return field;
}


function elem(id) {
  return document.getElementById(id);
}
function $elem(id) {
  return $(elem(id));
}

//Find filed templates in html document and precompile them.
function fieldTemplates() {
  return _.reduce(
    $(".field-template"),
    function(res, tmp) {
      res[/\w+/.exec(tmp.id)] = _.template($(tmp).text());
      return res;
    },
    {});
}

