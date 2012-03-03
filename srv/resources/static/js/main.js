/// Page layout rendering, loading models.

/// Setup routing
$(function(){
    var Pages = {
        "case": 
            {
                "template": "case-page-template",
                "views":
                    {
                        "left": renderCase
                    }
            },
        "search":
            {
                "template": "searchLayout",
                "views":
                    {
                        "main": renderSearch
                    }
            }
    }

    var MenuRouter = Backbone.Router.extend({
        routes: {
            "case/:id/": "loadCase",
            "case/":     "newCase"
        },

        loadCase: function (id) {
            loadCase(m);
        },
        
        newCase: function (m, id) {
            if ("case" == global.modelName)
                restore(id);
            else
                loadModel(m, id);
        }
    });    

    window.global = {
        formElement: $("#form"),
        knockVM: {},
        loadedModel: {},
        modelName: null,
        mkBackboneModel: null,
        router: new MenuRouter,
        timeliner: null,
    };

    Backbone.history.start({pushState: true});

});


//FIXME: redirect somewhere when `pageModel` is undefined?
function renderPage(pageModel) {
  // remove all forms from all containers
  $("#left, #right").children().detach();

  _.each(pageModel, function(containerModels, containerId) {
      _.each(containerModels, function(formId) {
        var form = createForm(formId, global.meta.form[formId]);
        $("#"+containerId).append(form);
      });
  });
  ko.applyBindings(global.viewModel);
}


function initOSM() {
      window.osmap = new OpenLayers.Map("basicMap");
      var mapnik = new OpenLayers.Layer.OSM();
      osmap.addLayer(mapnik);
      osmap.setCenter(new OpenLayers.LonLat(37.617874,55.757549) // Center of the map
        .transform(
          new OpenLayers.Projection("EPSG:4326"), // transform from WGS 1984
          new OpenLayers.Projection("EPSG:900913") // to Spherical Mercator Projection
        ), 16 // Zoom level
      );
}


/// Model functions.

// Model method HTTP access point wrt redson location
function modelMethod(modelName, method) {
    return "/_/" + modelName + "/" + method;
}

// Load model definition, set up globals and timeline updater. If id
// is not null, render form for instance, otherwise render empty
// form. Highlight active model menu item.
function loadModel(modelName, id) {
    $("#menu-" + global.modelName).removeClass("active");
    $("#menu-" + modelName).addClass("active");

    $.getJSON(modelMethod(modelName, "model"),
              function(model) {
                  global.loadedModel = model;
                  global.modelName = modelName;
                  global.mkBackboneModel = backbonizeModel(model, modelName);
                  $("#model-name").text(model.title);

                  if (_.isNull(global.timeliner))
                      global.timeliner = window.setInterval(refreshTimeline, 5000);

                  var idHash = {};
                  if (id)
                      idHash = {id: String(id)}
                  setupView(new global.mkBackboneModel(idHash));
              });
}

// Delete form, release observables
function forgetView() {
    kb.vmRelease(global.knockVM);
    global.formElement.empty();
}

// Render form for model and bind it
function setupView(instance) {
    global.knockVM = new kb.ViewModel(instance);

    refreshTimeline();

    global.formElement.html(renderFormView(global.loadedModel));
    ko.applyBindings(global.knockVM);

    // Wait a bit to populate model fields and bind form elements
    // without PUT-backs to server
    window.setTimeout(function () {
        global.knockVM._kb_vm.model.setupServerSync();
    }, 1000);
}

// Save current model instance
function save() {
    global.knockVM._kb_vm.model.save();
}

// Save current model instance and start fresh form
function proceed() {
    save();
    forgetView();
    setupView(new global.mkBackboneModel);
}

// Load existing model instance
function restore(id) {
    forgetView();
    setupView(new global.mkBackboneModel({"id": String(id)}));
}

// Remove currently loaded instance from storage and start fresh form
function remove(id) {
    global.knockVM._kb_vm.model.destroy();
    forgetView();
    setupView(new global.mkBackboneModel);
}
