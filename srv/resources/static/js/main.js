/// Screen layout rendering, loading models.

$(function(){
    // Screens have top-level template and a number of views.
    //
    // Each view has setup function which accepts DOM element, screen
    // arguments and renders HTML to element, and sets up viewsWare
    // value for its view element. View name matches the ID of
    // enclosing DOM element ($el(<viewName>) = viewName's DOM).
    //
    // View may have no standard setup function, in which case it must
    // be called from outside (e.g. after user took action in a
    // different view). In this case noOp must be selected as view
    // function.
    //
    // Screen rendering is called through router.
    var Screens = {
        "case":
            {
                "template": "case-screen-template",
                "views":
                    {
                        "form": setupCaseMain,
                        "subform": noOp
                    }
            },
        "search":
            {
                "template": "search-screen-template",
                "views":
                    {
                        "tableView": setupSearchTable
                    }
            }
    };

    // Setup routing
    var MenuRouter = Backbone.Router.extend({
        routes: {
            "case/:id/": "loadCase",
            "case/": "newCase",
            "search/": "search"
        },

        loadCase: function (id) {
            renderScreen("case", {"id": id});
        },

        newCase: function () {
            renderScreen("case", {"id": null});
        },

        search: function () {
            renderScreen("search");
        }
    });

    window.global = {
        // «Screen» element which holds all views
        topElement: $el("layout"),
        screens: Screens,
        router: new MenuRouter,

        activeScreen: null,
        // viewWare is for bookkeeping of views in current screen.
        //
        // Hash keys are DOM tree element IDs associated with the
        // model (view names). Values are hashes which contain the
        // following keys:
        //
        // - model (model definition);
        // - modelName;
        // - mkBackboneModel (Backbone constructor);
        // - knockVM (Knockout ViewModel bound to view).
        //
        // When screen is loaded, viewsWare should generally contain
        // only keys which correspond to that screen views. View
        // renderers maintain their viewsWare.
        viewsWare: {}
    };
    // TODO Fix router to actually work
    renderScreen("search");

    Backbone.history.start({pushState: false});
});

function el(id) {
    return document.getElementById(id);
}

function $el(id) {
    return $(el(id));
}

// Render top-level screen template (static)
//
// args object is passed further to all view setup functions.
function renderScreen(screenName, args) {
    forgetScreen();
    var screen = global.screens[screenName];
    global.activeScreen = screen;

    // Highlight the new item in navbar
    $("li.active").removeClass("active");
    $el(screenName + "-screen-nav").addClass("active");

    var tpl = $el(screen.template).html();
    global.topElement.html(tpl);
    // Call setup functions for all views, assuming they will set
    // their viewsWare
    for (viewName in screen.views) {
        screen.views[viewName](viewName, args);
    }
}

// Remove all content of view and clean up wares.
//
// To setup view back again, call
// screen.views[viewName]($el(viewName), args);
function forgetView(viewName) {
    var vW = global.viewsWare[viewName];
    // View may have not setup any knockVM (static views like search)
    if (!_.isUndefined(vW.knockVM))
        kb.vmRelease(vW.knockVM);
    vW = {};
    $el(viewName).empty();
}

// Clean up all views on screen and everything.
function forgetScreen() {
    for (viewName in global.viewsWare) {
        forgetView(viewName);
    };
    global.topElement.empty();
    global.viewsWare = {};
    global.activeScreen = null;
}

// Dumb setup for setupless views
function noOp(viewName, args) {};

/// Model functions.

// Return function which will setup views for that model given its
// view name and instance id. Standard Backbone-metamodel renderer
// is used to generate HTML contents in element.
function modelSetup(modelName) {
    return function(viewName, id) {
        $.getJSON(modelMethod(modelName, "model"),
            function(model) {
                mkBackboneModel = backbonizeModel(model, modelName);
                var idHash = {};
                if (id)
                    idHash = {id: String(id)}
                instance = new mkBackboneModel(idHash);
                knockVM = new kb.ViewModel(instance);

                $el(viewName).html(renderFormView(model, viewName));
                ko.applyBindings(knockVM, el(viewName));

                // Wait a bit to populate model fields and bind form
                // elements without PUT-backs to server
                //
                // TODO First POST is still broken somewhy.
                window.setTimeout(function () {
                    knockVM._kb_vm.model.setupServerSync();
                }, 1000);

                global.viewsWare[viewName] = {
                    "model": model,
                    "modelName": modelName,
                    "mkBackboneModel": mkBackboneModel,
                    "knockVM": knockVM
                };
            });
    }
}

// Model method HTTP access point wrt redson location
function modelMethod(modelName, method) {
    return "/_/" + modelName + "/" + method;
}

// Save instance loaded in view
function saveInstance(viewName) {
    global.viewsWare[viewName].knockVM._kb_vm.model.save();
}

// Load existing model instance
function createInstance(viewName, id) {
    saveInstance(viewName);
    forgetView(viewName);
    global.activeScreen.views[viewName](viewName, {});
}

// Load existing model instance
function restoreInstance(viewName, id) {
    forgetView(viewName);
    global.activeScreen.views[viewName](viewName, {"id": id});
}

// Remove instance currently loaded in view from storage and render
// that view from scratch (if possible)
function removeInstance(viewName) {
    global.viewsWare[viewName].knockVM._kb_vm.model.destroy();
    forgetView(viewName);
    var setup = global.activeScreen.views[viewName];
    if (!_.isUndefined(setup))
        setup(viewName, {});
}


/// View setup functions.

// Case view
function setupCaseMain(viewName, args) {
    return modelSetup("case")(viewName, args.id);
}

// Show service in subform. Reference is '<modelname>:<id>'
//
// How to update parent reference value when new service is created?
function setupService(viewName, reference) {
    // O_o
    var slices = /(\w+):(\w+)/.exec(reference);
    var model = slices[1];
    var id = slices[2];
    return modelSetup(model)(viewName, id);
}

// Search main view
function setupSearchTable(viewName, args) {
    $el(viewName).html($el("search-table-template").html());
    global.viewsWare[viewName] = {};
    $el("searchtable").dataTable({
        aoColumnDefs: [{
            // Render case id as link to case page
            fnRender: function (o, val) {
                return "<a href='#' onclick=\"renderScreen('case', {id:" +
                    val + "});\">" + val + "<a/>";
            },
            aTargets: [0]
        }],
        oLanguage: {
            sSearch: "Фильтр",
            oPaginate: {
                sNext: "Вперёд",
                sPrevious: "Назад"
            },
            sInfo: "Показаны записи с _START_ по _END_ (всего _TOTAL_)",
            sInfoEmpty: "",
            sZeroRecords: "Ничего не найдено"
        }});
}

// Manually load JSON data from server and add it to table
//
// TODO Allow adjusting of search fields etc.
function doSearch() {
    var fields = ["id", "ownerName", "callDate", "phone", "plateNum", "program"];
    var sType = "or"
    var limit = "100";

    var t = $el("searchtable").dataTable();
    var term = $el("table-query").val();
    var method = "search/?";
    for (f in fields) {
        method += (fields[f] + "=" + term + "&");
    };
    method += "_matchType=s&_searchType=" + sType + "&_limit=" + limit;
    $.getJSON(modelMethod("case", method),
              function(results) {
                  var data = []
                  for (i = 0; i < results.length; i++) {
                      var o = results[i];
                      var row = [];
                      // Extract fields data from JSON, show dash in
                      // case of undefined field
                      for (j = 0; j < fields.length; j++) {
                          if (_.isUndefined(o[fields[j]]))
                              row[j] = "—";
                          else
                              row[j] = o[fields[j]];
                      }
                      data[i] = row;
                  }
                  t.fnClearTable();
                  t.fnAddData(data);
              });
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
