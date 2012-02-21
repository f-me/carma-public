$(function(){
    window.global = {
      fieldTemplate: getFieldTemplates(),
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
      _.each(containerModels, function(formName) {
        var form = global.view[formName];
        if (_.isUndefined(form)) {
          var res = createForm(formName, global.meta.form[formName]);

          global.view[formName] = res.form;
          global.viewModel[formName] = res.viewModel; 
          form = res.form;

          //I'm applying bindings only to the body to prevent ko from binding
          //something in field templates (they are placed outside of the body element).
          //FIXME: It may be that rebinding everyting on each form creation is
          //not vey efficient, but I don't know how to apply only new viewModel
          //without breaking everyting else.
          ko.applyBindings(global.viewModel, $("body")[0]);
        }
        $("#"+containerId).append(form);
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
        var field = createField(formId, fieldId, fieldMeta);
        field.appendTo(form);
        field = field.find(".field");

        vm[fieldId] = ko.observable(fieldMeta.default);
      } catch(e) {
        console.log(e);
      }
    });
  });

  return {form:form, viewModel:vm};
}

function createField(formId,fieldId,fieldMeta) {
  //apply defaults to filed description
  fieldMeta = _.defaults(fieldMeta, {
    type: "text",
    id: formId + "." + fieldId,
    default:""
  });

  //apply field template to field description to create
  //corresponding html element
  var field = $(global.fieldTemplate[fieldMeta.type](fieldMeta));
  return field;
}


//Find filed templates in html document and precompile them.
function getFieldTemplates() {
  return _.reduce(
    $(".field-template"),
    function(res, tmp) {
      res[/\w+/.exec(tmp.id)] = _.template($(tmp).text());
      return res;
    },
    {});
}

