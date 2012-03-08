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


// A helper to be used as referenced instance's fetchCb.
//
// It will add a reference to named field of parent instance when it
// becomes available from referenced instance. (After first POST
// usually.)
// 
// We heard you like callbacks...
function mkRefFetchCb(parentInstance, field) {
    var fetchCb = function(refInstance) {
        var idChangeCb = function () {
            if (refInstance.hasChanged("id")) {
                refInstance.unbind("change", idChangeCb);

                var newRef = refInstance.name + ":" + refInstance.id;
                var newValue;
                var oldValue = parentInstance.get(field);
                if (_.isNull(oldValue) || (oldValue == ""))
                    newValue = oldValue;
                else
                    newValue = oldValue + "," + newRef;
                var hash = {};
                hash[field] = newValue;
                parentInstance.set(hash);
            }
        }
        // Have to use cb inside a cb becase fetchCb itself gets
        // unbound after first change.
        refInstance.bind("change", idChangeCb);
    }
    return fetchCb;
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
// {services: [{refN: 0, refModel: "towage", refId: "2131"},..]}
//
// means that field services contained a reference to instance of
// "towage" model with id "2131" and view "services-view-0" was
// generated for it. If refFields was {"services": "foo"}, then view
// was placed inside "foo" container.
function setupRefs(instance, refFields) {
    var tpls = getTemplates("reference-template");
    
    var books = {};

    for (rf in refFields) {
        books[rf] = [];
        // Generate a bunch of views to fill in
        var refs = instance.get(rf);
        if (!_.isNull(refs) && (refs != "")) {
            refs = refs.split(",");
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

                referenceViews += renderRef(refBook, tpls);
            }
            $el(refFields[rf]).html(referenceViews);
        }
    }
    return books;
}

// Generate HTML contents for view which will be populated by
// referenced instance described keys by refBook:
//
// refN - number of instance in reference field of parent model;
// refModel - model being referenced;
// refId - id of model instance being referenced;
// refField - name of field of parent model which stores reference;
//
// refBook may contain any other keys as well and will be passed to
// Mustache.render as a context.
//
// Templates will be pickTemplate'd against using either
// <refModel>-<refField>, simply <refField> or default template.
//
// Every view template must set div with id=<refField>-view-<refN>,
// where instance will be rendered after loading.
function renderRef(refBook, templates) {
    var named_tpl = refBook.refModel + "-" + refBook.refField;
    var typed_tpl = refBook.refField;
    return Mustache.render(pickTemplate(templates, 
                                        [named_tpl, typed_tpl, ""]),
                           refBook);
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
// Permissions template for model is rendered in element permEl.
//
// slotsee is an array of element IDs:
//
// [foo-title", "overlook"]
//
// which will by ko.applyBindings'd to with model after it's finished
// loading, in addition to elName.
//
// fetchCb is a function to be called with Backbone instance as
// argument after it emits first "change" event (which is usually
// initial fetch() of data stored on server). Use this to generate
// views for references.
function modelSetup(modelName) {
    return function(elName, id, fetchCb, slotsee, permEl) {
        $.getJSON(modelMethod(modelName, "model"),
            function(model) {
                var mkBackboneModel = backbonizeModel(model, modelName);
                var idHash = {};
                if (id)
                    var idHash = {id: String(id)}

                var instance = new mkBackboneModel(idHash);

                if (_.isFunction(fetchCb)) {
                    // Do the same for all refFields after first fetch()
                    // is complete
                    var fetchCallback;
                    fetchCallback = function () {
                        // Just once
                        instance.unbind("change", fetchCallback);
                        fetchCb(instance);
                    }
                    instance.bind("change", fetchCallback);
                }
                var knockVM = knockBackbone(instance);

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
                    instance.setupServerSync();
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
