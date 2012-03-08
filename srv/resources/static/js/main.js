/// Screen layout rendering, loading models, frobnicating foobars.
//
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
//
// Dictionaries are used by UI to map predefined keys to readable
// values.
function mainSetup(localScreens, localRouter, localDictionaries) {
    var Screens = localScreens;

    window.global = {
        // «Screen» element which holds all views
        topElement: $el("layout"),
        screens: Screens,
        router: new localRouter,
        dictionaries: localDictionaries,

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
        // - bbInstance (Backbone model);
        // - knockVM (Knockout ViewModel bound to view).
        //
        // When screen is loaded, viewsWare should generally contain
        // only keys which correspond to that screen views. View
        // renderers maintain their viewsWare.
        viewsWare: {}
    };
    Backbone.history.start({pushState: false});
}

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


function knockBackbone(instance) {
    var knockVM = new kb.ViewModel(instance);
    return knockVM;
}

/// Model functions.

// Return function which will setup views for that model given its
// form element name and instance id. Standard Backbone-metamodel
// renderer is used to generate HTML contents in form view. viewsWare
// is updated properly after the model loading is finished.
//
// Following keys are recognized in options argument:
// 
// - permEl: string, name of element to render permissions template
// - for model into;
//
// - slotsee: array of element IDs:
//
//   [foo-title", "overlook"]
//
//   which will be ko.applyBindings'd to with model after it's
//   finished loading, in addition to elName;
//
// - fetchCb: function to be bound to "change" event of Backbone
//   instance. Use this to generate views for references;
//
// - focusClass: class of form field items; if used, first item of
//   this class will get focus after form render.
function modelSetup(modelName) {
    return function(elName, id, options) {
        $.getJSON(modelMethod(modelName, "model"),
            function(model) {
                var mkBackboneModel = backbonizeModel(model, modelName);
                var idHash = {};
                if (id)
                    var idHash = {id: String(id)}

                var instance = new mkBackboneModel(idHash);

                if (_.isFunction(options.fetchCb)) {
                    instance.bind("change", options.fetchCb);
                }
                var knockVM = knockBackbone(instance);

                $el(elName).html(renderFields(model, elName));
                $el(options.permEl).html(renderPermissions(model, elName));
                ko.applyBindings(knockVM, el(elName));

                for (s in options.slotsee) {
                    ko.applyBindings(knockVM, el(options.slotsee[s]));
                }

                // Wait a bit to populate model fields and bind form
                // elements without PUT-backs to server
                //
                // TODO First POST is still broken somewhy.
                window.setTimeout(function () {
                    instance.setupServerSync();
                }, 1000);

                if (options.focusClass)
                    $el(elName).find("." + options.focusClass)[0].focus();

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
    global.viewsWare[viewName].bbInstance.save();
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


/// Reference fiddlage routines

// A helper to be used as referenced instance's fetchCb.
//
// It will add a reference to named field of parent instance when it
// becomes available from referenced instance. (After first POST
// usually.)
// 
// We heard you like callbacks...
function mkRefFetchCb(parentInstance, field) {
    var fetchCb = function(refInstance) {
        if (refInstance.hasChanged("id")) {
            refInstance.unbind("change", fetchCb);
            var newRef = refInstance.name + ":" + refInstance.id;
            var newValue;
            var oldValue = parentInstance.get(field);
            if (_.isNull(oldValue) || (oldValue == ""))
                newValue = newRef;
            else
                newValue = oldValue + "," + newRef;
            var hash = {};
            hash[field] = newValue;
            parentInstance.set(hash);
        }
    }
    return fetchCb;
}

// Setup views for references stored in given instance field.
//
// We generate a forest of views in element refsForest where each view
// has id in form of <rf>-view-<N>, N = 0,1.. and class <rf>-view.
// renderRef is used to build every view.
//
// Return array with book for every view generated this way. Every
// book is an object with keys refN, refModel, refId.
//
// Example: [{refN: 0, refModel: "towage", refId: "2131"},..]
//
// means that instance field contained a reference to instance of
// "towage" model with id "2131" and view "services-view-0" was
// generated for it. If refsForest was "foo", then view was placed
// inside "foo" container.
//
// After views have been rendered, collected books may be used to
// perform a modelSetup() inside every view.
//
// If refField is empty in instance, nothing is rendered.
function setupMultiRef(instance, refField, refsForest) {
    var tpls = getTemplates("reference-template");
    
    var books = {};

    var refs = instance.get(refField);
    if (!_.isNull(refs) && (refs != "")) {
        refs = refs.split(",");
        var referenceViews = "";
        for (i in refs) {
            var slices = /(\w+):(\w+)/.exec(refs[i]);
            var model = slices[1];
            var id = slices[2];

            var refBook = {refN: i,
                           refModelName: model,
                           refId: id,
                           refField: refField};
            books[i] = refBook;

            referenceViews += renderRef(refBook, tpls);
        }
        $el(refsForest).append(referenceViews);
    }
    return books;
}

// Add new reference to instance, render view and form for it.
//
// View template used 
//
// instance is a Backbone model, refField is the name of instance
// field to store reference in, refModelName sets name of model to
// create reference to, refsForest is ID of element which holds views
// for services of model.
function addReference(instance, refField, refModelName, refsForest) {
    var oldValue = instance.get(refField);
    var tpls = getTemplates("reference-template");
    if (_.isNull(oldValue) || (oldValue == ""))
        oldValue = [];
    else
        oldValue = oldValue.split(",");

    // Cannot check against oldService.length because there may be
    // more than 1 unsaved service
    var refN = $("." + refField + "-view").length;

    // Render view
    var html = renderRef({refN: refN,
                           refModelName: refModelName,
                           refId: null,
                           refField: refField},
                        tpls);
    $el(refsForest).append(html);
    var fetchCb = mkRefFetchCb(instance, refField);
    var subview = refField + "-view-" + refN;

    modelSetup(refModelName)(subview, null, 
                             {fetchCb: fetchCb,
                              slotsee: [subview + "-link"],
                              permEl: subview + "-perms",
                              focusClass: "focusable"});
}

// Generate HTML contents for view which will be populated by
// referenced instance described by keys of refBook:
//
// refN - number of instance in reference field of parent model;
// refModelName - model being referenced;
// refId - id of model instance being referenced;
// refField - name of field of parent model which stores reference;
//
// refBook may contain any other keys as well and will be passed to
// Mustache.render as a context.
//
// Templates will be pickTemplate'd against using
// <refModelName>-<refField>, simply <refField> or default template.
//
// Every view template must set div with id=<refField>-view-<refN>,
// where instance will be rendered after loading.
function renderRef(refBook, templates) {
    var named_tpl = refBook.refModelName + "-" + refBook.refField;
    var typed_tpl = refBook.refField;
    return Mustache.render(pickTemplate(templates, 
                                        [named_tpl, typed_tpl, ""]),
                           refBook);
}
