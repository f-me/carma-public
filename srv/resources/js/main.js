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
        createForm(formId, global.meta.form[formId]);
        $("#"+containerId).append(global.view[formId]);
      });
  });

  _.each(pageModel, function(containerModels, containerId) {
      _.each(containerModels, function(formId) {
        processFormDependencies(formId,global.meta.form[formId]); 
      });
  });

  ko.applyBindings(global.viewModel);
}


function createForm(formId, formMeta) {
  var form = $("<fieldset/>");
  var vm = {};

  global.view[formId] = form;
  global.viewModel[formId] = vm; 

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

        if (fieldMeta.exposeId) {
          field.attr("id", formId+"-"+fieldId);
        }
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
}

//FIXME: OMG! seven levels of indentation!
//And the code is quite ugly by itself.
function processFormDependencies(formId, formMeta) {
  _.each(formMeta.dependencies,function(depMeta, targetId) {
      if (_.has(depMeta, "value")) {
        var srcVM = _.reduce(
          depMeta.dependsOn.split("."),
          function(res,p) { return res[p]; },
          global.viewModel);

        var tgtId = "#"+formId+"-"+targetId;
        srcVM.subscribe(function(val) {
          var tgtElem = $(tgtId);
          var formId = depMeta.value[val];
          if (!_.has(global.view, formId)) {
            createForm(formId, global.meta.form[formId]);
          }

          var form = global.view[formId];
          tgtElem.children().detach();
          tgtElem.append(form);
          ko.applyBindings(global.viewModel, form[0]);
        });

        srcVM.notifySubscribers(srcVM());
      }
  });
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

