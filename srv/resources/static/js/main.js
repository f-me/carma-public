/// Screen layout rendering, loading models, frobnicating foobars.
///
/// localScreens and localRouter must be set prior to loading this
/// module.


$(function(){
    // Screens have top-level template and a number of views.
    //
    // Each view has setup function which accepts DOM element, screen
    // arguments and renders HTML to element, and sets up viewsWare
    // value for its view element. View name matches the ID of
    // enclosing DOM element ($el(<viewName>) = viewName's DOM).
    //
    // In case view has no standard setup function, null must be
    // specified instead.
    //
    // Screen rendering is called through router.
    var Screens = localScreens;

    window.global = {
        // «Screen» element which holds all views
        topElement: $el("layout"),
        screens: Screens,
        router: new localRouter,

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
        var setup = screen.views[viewName];
        if (!_.isNull(setup))
            setup(viewName, args);
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

// Setup views for references stored in given instance fields.
//
// For every refFields rf key, generate a forest of views in element
// refFields[rf] where each has id in form of <rf>-view-<N>, N = 0,1..
// and class <rf>-view.
//
// Return hash with books of views generated this way for every rf.
// Every book contains array of object with keys refN, refModel,
// refId.
//
// Example:
// {service: [{refN: 0, refModel: "towage", refId: "2131"},..]}
//
// means that field service contained a reference to instance of
// "towage" model with id "2131" and view "service-view-0" was
// generated for it. If refFields was {"service": "foo"}, then view
// was placed inside "foo" container.
function setupRefs(instance, refFields) {
    var tplNs = "reference-template";
    var tpls = getTemplates(tplNs);
    
    var books = {};

    for (rf in refFields) {
        books[rf] = [];
        // Generate a bunch of views to fill in
        var refs = instance.get(rf).split(",");
        if (refs[0] != "") {
            var referenceViews = "";
            for (i in refs) {
                var slices = /(\w+):(\w+)/.exec(refs[i]);
                var model = slices[1];
                var id = slices[2];

                var named_tpl = model + "-" + rf;
                var typed_tpl = rf;
                var refBook = {refN: i,
                               refModel: model,
                               refId: id,
                               refField: rf};
                books[rf][i] = refBook;

                // Every view must set div with
                // id=<refField>-view-<refN>
                referenceViews
                    += Mustache.render(pickTemplate(tpls, [named_tpl, typed_tpl, ""]),
                                       refBook);
            }
            $el(refFields[rf]).html(referenceViews);
        }
    }
    return books;
}

/// Model functions.

// Return function which will setup views for that model given its
// form element name and instance id. Standard Backbone-metamodel
// renderer is used to generate HTML contents in form view. viewsWare
// is updated properly after the model loading is finished.
//
// Permissions template for model is rendered in element permEl.
//
// multirefs argument is a hash where each key is the name of field
// with type reference to be rendered immediately after the parent
// model has been loaded and value is the element name to render
// forest of reference templates into. No way to GO DEEPER yet.
//
// Example:
// refFields = {"service": "bar-baz"}
// 
// will render a bunch of views for references stored in "service"
// field of model in element with id "bar-baz". Referenced instances
// are rendered with modelSetup as well which means that viewsWare
// will be used for further proper cleanup. Slotsee for every Nth
// referenced instance of field is set to {{field}}-view-{{refN}}-link
// (to be used as insight into referenced instance) and permissions
// will be rendered into {{field}}-view-{{refN}}-perms.
//
// We must render reference views after the model has loaded because
// the numer of refs is unknown when the model has not yet been
// populated with data.
//
// slotsee is an array of element IDs:
//
// [foo-title", "overlook"]
//
// which will by ko.applyBindings'd to with model after it's finished
// loading, in addition to elName.
function modelSetup(modelName, refFields) {
    return function(elName, id, slotsee, permEl) {
        $.getJSON(modelMethod(modelName, "model"),
            function(model) {
                mkBackboneModel = backbonizeModel(model, modelName);
                var idHash = {};
                if (id)
                    idHash = {id: String(id)}

                instance = new mkBackboneModel(idHash);

                // Do the same for all refFields after first fetch()
                // is complete
                var fetchCallback;
                fetchCallback = function () {
                    // Just once
                    instance.unbind("change", fetchCallback);
                    
                    books = setupRefs(instance, refFields);
                    for (rf in books) {
                        for (rn in books[rf]) {
                            var subview = rf + "-view-" + rn;
                            var setup = modelSetup(books[rf][rn].refModel, {});
                            setup(rf + "-view-" + rn, books[rf][rn].refId,
                                  [subview + "-link"],
                                  subview + "-perms");
                        }
                    }
                }
                instance.bind("change", fetchCallback);

                knockVM = new kb.ViewModel(instance);

                $el(elName).html(renderFields(model, elName));
                $el(permEl).html(renderPermissions(model, elName));
                ko.applyBindings(knockVM, el(elName));

                for (s in slotsee) {
                    ko.applyBindings(knockVM, el(slotsee[s]));
                }

                // Wait a bit to populate model fields and bind form
                // elements without PUT-backs to server
                //
                // TODO First POST is still broken somewhy.
                window.setTimeout(function () {
                    knockVM._kb_vm.model.setupServerSync();
                }, 1000);

                global.viewsWare[elName] = {
                    "model": model,
                    "bbInstance": instance,
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
    if (!_.isNull(setup))
        setup(viewName, {});
}
