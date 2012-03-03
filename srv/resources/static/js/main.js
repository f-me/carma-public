/// Screen layout rendering, loading models.
///
/// We assume that two models can be loaded at once (main and child).

$(function(){
    /// Screens have top-level template and a number of views.
    var Screens = {
        "case": 
            {
                "template": "case-screen-template",
                "views":
                    {
                        "left": renderCase
                    }
            },
        "search":
            {
                "template": "search-screen-template",
                "views":
                    {
                        "main": renderSearch
                    }
            }
    }

    /// Setup routing
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
        // Hash keys are DOM tree element IDs associated with the
        // model (view names). Values are hashes which contain the
        // following keys:
        //
        // - model (model definition);
        // - modelName;
        // - mkBackboneModel (Backbone constructor);
        // - knockVM (Knockout ViewModel bound to view).
        views: {},
        screens: Screens,
        activeScreen: null,
        topElement: $el("layout"),
        router: new MenuRouter, 
   };

    Backbone.history.start({pushState: true});
});

function el(id) {
    return document.getElementById(id);
}

function $el(id) {
    return $(el(id));
}

// Release observables, clean up everything
function forgetScreen() {
    for (view in global.views) {
        kb.vmRelease(view.knockVM);
        $elem("view").empty();
    };
    global.activeScreen = null;
}

// Render top-level screen template (static)
function renderScreen(screen) {
    var tpl = $el(screen.template).html();
    global.topElement.html(tpl);
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

// Load model definition, set up globals. 

// If id is not null, render form for instance, otherwise render empty
// form.
function loadModel(modelName, id) {
    $.getJSON(modelMethod(modelName, "model"),
              function(model) {
                  global.loadedModel = model;
                  global.modelName = modelName;
                  global.mkBackboneModel = backbonizeModel(model, modelName);
                  $("#model-name").text(model.title);

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
