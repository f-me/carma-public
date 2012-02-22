$(function(){
    window.global = {
      viewPlugin: viewPlugins(),
      fieldTemplate: fieldTemplates(),
      meta: {
        page: metaPages(),
        form: metaForms(),
      },
      view: {},
      viewModel:{}
    };

    var menuRouter = initBottomMenu();
    menuRouter.bind("route:updMenu", function(sect) {
      renderPage(global.meta.page[sect]); //FIXME: injection //TODO: pass `arguments`
    });

    Backbone.history.start({pushState: true});

    $(".field:first").focus();
});


//FIXME: redirect somewhere when `pageModel` is undefined?
function renderPage(pageModel) {
  // remove all forms from all containers
  $(".column").children().detach();

  _.each(pageModel, function(containerModels, containerId) {
      _.each(containerModels, function(formId) {
        if (_.isUndefined(global.view[formId])) {
          var formMeta = global.meta.form[formId];
          createForm(formId, formMeta);
        }
        $("#"+containerId).append(global.view[formId]);

        //FIXME:I'm applying bindings only to the body to prevent ko from
        //binding something in field templates (they are placed outside of the
        //body element).
        //FIXME: It may be that rebinding everyting on each form creation is
        //not vey efficient, but I don't know how to apply only new viewModel
        //without breaking everyting else.
        ko.applyBindings(global.viewModel, $("body")[0]);
        processFormDependencies(formId,formMeta); 
      });
  });
}


function createForm(formId, formMeta) {
  var form = $("<fieldset/>");
  var vm = {};

  if (_.has(formMeta, "title")) {
    form.append("<legend>" + formMeta.title + "</legend>");
  }

  _.each(formMeta.fields, function(f) {
    _.each(f, function(fieldMeta, fieldId) {
      try {
        //apply defaults to filed description
        fieldMeta = _.defaults(fieldMeta, {
          type: "text",
          id: formId + "." + fieldId,
          default: ""
        });

        //apply field template to field description to create
        //corresponding html element
        var field = $(global.fieldTemplate[fieldMeta.type](fieldMeta));

        // var field = createField(formId, fieldId, fieldMeta);

        field.appendTo(form);
        field = field.find(".field");
        //apply additional plugins
        _.each(fieldMeta,function(val,key) {
            if (_.has(global.viewPlugin, key)) {
              global.viewPlugin[key](field, fieldMeta);
            }
        });

        vm[fieldId] = ko.observable(fieldMeta.default);
      } catch(e) {
        console.log(e);
      }
    });
  });

  global.view[formId] = form;
  global.viewModel[formId] = vm; 
}

//FIXME: OMG! seven levels of indentation!
//And the code is quite ugly by itself.
function processFormDependencies(formId, formMeta) {
  _.each(formMeta.dependencies,function(depMeta, fieldId) {
    _.each(depMeta.type, function(params, depType) {
      if (depType === "append") {
        var containerId = params;
        var fieldVM = global.viewModel[formId][fieldId];
        //Here is the closure that we use to store link to the current form
        //element
        (function() {
          var form;
          fieldVM.subscribe(function(val) {
            if (!_.isUndefined(form)) {
              form.detach();
            }
            var depId = depMeta.value[val];
            if (!_.has(global.view, depId)) {
              createForm(depId, global.meta.form[depId]);
            }
            form = global.view[depId];
            $("#"+containerId).append(form);
            //FIXME: see note above about applyBindings
            ko.applyBindings(global.viewModel, $("body")[0]);
          });
        })();

        fieldVM.notifySubscribers(fieldVM());
      }
    });
  });
}


function createField(formId,fieldId,fieldMeta) {
  //apply defaults to filed description
  fieldMeta = _.defaults(fieldMeta, {
    type: "text",
    id: formId + "." + fieldId,
    default: ""
  });

  //apply field template to field description to create
  //corresponding html element
  var field = $(global.fieldTemplate[fieldMeta.type](fieldMeta));

  return field;
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

