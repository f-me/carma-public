$(function(){
    global = {
      meta: {
        page: metaPages(),
        form: metaForms(),
        fieldTemplate: getFieldTemplates()
      },
      ui: {form:{}}
    };

    var menuRouter = initBottomMenu();
    menuRouter.bind("route:updMenu", function(sect) {
      renderPage(global.meta.page[sect]); //FIXME: injection //TODO: pass `arguments`
    });

    Backbone.history.start({pushState: true});

    $(".field:first").focus();
});


function renderPage(pageModel) {
  _.each(pageModel, function(containerModels, containerId) {
      var cont = $("#"+containerId);
      cont.children().detach(); //remove old forms from container

      _.each(containerModels, function(formName) {
        var form = global.ui.form[formName];
        if (_.isUndefined(form)) {
          form = createForm(formName, global.meta.form[formName]);
          global.ui.form[formName] = form;
        }
        cont.append(form); //add new form to container
      });
  });
}


function createForm(formName, formMeta) {
  var form = $("<fieldset/>");
  if (_.has(formMeta, "title")) {
    form.append("<legend>" + formMeta.title + "</legend>");
  }

  _.each(formMeta.fields, function(f) {
    _.each(f, function(fieldMeta, fieldId) {
      //apply defaults to filed description
      fieldMeta = _.defaults(fieldMeta, {
        type: "text",
        id: formName + "." + fieldId
      });

      //apply field template to field description to create
      //corresponding html element
      var field = $(global.meta.fieldTemplate[fieldMeta.type](fieldMeta));
      field.appendTo(form);
      field = field.find(".field");
    });
  });

  return form;
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

