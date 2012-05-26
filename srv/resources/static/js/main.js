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
// values (see knockBackbone).
//
// modelHooks — a hash with lists of hooks called at the end of
// modelSetup for respective model.
//
// user object is stored in global hash and contains data about
// current user.
function mainSetup(localScreens, localRouter, localDictionaries, modelHooks, user) {
    var Screens = localScreens;

    var dictLabelCache = {};
    var dictValueCache = {};

    // Build caches (TODO: Do this on server some day)
    for (d in localDictionaries) {
        dictLabelCache[d] = {};
        dictValueCache[d] = {};
        var dict = localDictionaries[d];
        if (_.isArray(dict.entries))
            for (e in dict.entries) {
                var l = dict.entries[e].label;
                var v = dict.entries[e].value;
                dictLabelCache[d][l] = v;
                dictValueCache[d][v] = l;
            }
        else
            for (c in dict.entries)
                for (e in dict.entries[c]) {
                    var l = dict.entries[c][e].label;
                    var v = dict.entries[c][e].value;
                    if (l && v) {
                        dictLabelCache[d][l] = v;
                        dictValueCache[d][v] = l;
                    }
                }
    }

    window.global = {
        // «Screen» element which holds all views
        topElement: $el("layout"),
        screens: Screens,
        router: new localRouter,
        dictionaries: localDictionaries,
        // Maps labels to values for every dictionary
        dictLabelCache: dictLabelCache,
        // Maps values to labels
        dictValueCache: dictValueCache,
        modelHooks: modelHooks,
        user: user,
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
        // - knockVM (Knockout ViewModel bound to view);
        // - depViews (hash with views for every reference/group field).
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


// Backbone-Knockout bridge
//
// Sets additional observables in Knockout model:
//
// - <field>Not for every required field;
// 
// - maybeId; («—» if Backbone id is not available yet)
//
// - modelTitle;
//
// - <field>Local for dictionary fields: reads as label, writes real
//   value back to Backbone model;
function knockBackbone(instance, viewName) {
    var knockVM = new kb.ViewModel(instance);

    // Set extra observable for inverse of every required
    // parameters, with name <fieldName>Not
    for (f in instance.requiredFields) {
        knockVM[instance.requiredFields[f] + "Not"] =
            kb.observable(instance,
                          {key: instance.requiredFields[f],
                           read: function (k) {
                               return !instance.get(k)
                           }});
    }

    // Set observable with name <fieldName>Regexp for inverse of
    // result of regexp checking for every field with meta.regexp
    // annotation. Observable is True when regexp fails.
    for (n in instance.regexpFields) {
        var fieldName = instance.regexpFields[n];
        var regexp = instance.fieldHash[fieldName].meta.regexp;
        (function(f, r) {
            knockVM[fieldName + "Regexp"] =
                kb.observable(instance,
                              {key: f,
                               read: function (k) {
                                   return !r.test(instance.get(k));
                               }});
        })(fieldName, new RegExp(regexp));
    }

    for (n in instance.dictionaryFields) {
        var fieldName = instance.dictionaryFields[n];
        var dict = instance.fieldHash[fieldName].meta.dictionaryName;
        var parent = instance.fieldHash[fieldName].meta.dictionaryParent;

        // Perform label-value transformation
        (function(f, d){
            knockVM[fieldName + "Local"] =
                kb.observable(instance,
                              {key: f,
                               read: function(k) {
                                   // Read label by real value
                                   var val = instance.get(k);
                                   var lab = global.dictValueCache[d][val];
                                   return (lab || val);
                               },
                               write: function(lab) {
                                   // Set real value by label
                                   var val = global.dictLabelCache[d][lab]
                                   instance.set(f, val || lab);
                               }
                              },
                              knockVM);
        })(fieldName, dict);
    }

    knockVM["modelTitle"] = kb.observable(instance,
                                          {key: "title",
                                           read: function (k) {
                                               return instance.title;
                                           }});
    knockVM["maybeId"] = 
        kb.observable(instance,
                      {key: "id",
                       read: function (k) {
                           return (instance.isNew()) ? "—" : instance.id;
                       }});

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
//   instance. Use this to update references of parent model when
//   referenced instance views are set up.
//
// - refs: Describe what references model has and where to render
//   their views. This key is an array of objects:
//
//   [{
//      field: "foo",
//      forest: "foo-subrefs"
//    },
//    {
//      field: "bar",
//      forest: "main-subref",
//      modelName: "fooReferencedModel"
//    }]
//
//   field sets the field of parent model where references are stored,
//   forest is the name of element to render views for references into.
//
//   Views generated for references are stored in viewsWare, so that
//   parent instance can get access to its reference views:
//
//   > viewsWare["parent-view"].depViews["some-ref-field"]
//   ["view-1", "view-2"]
//
//   etc., where "view-1" and "view-2" were generated for instances
//   which are referenced in "some-ref-field".
//
// - groupsForest: The name of forest where to render views for field
//   groups. Views generated for groups are stored in depViews under
//   viewsWare entry for parent view. Referenced models are
//   recursively rendered with the same value of groupsForest (so
//   parent model and its children share the same groupsForest).
//
// After model is set, every hook in global.modelHooks["*"] and
// global.modelHooks[modelName] is called with model view name as
// argument.
function modelSetup(modelName) {
    return function(elName, args, options) {
        $.getJSON(modelMethod(modelName, "model"),
            function(model) {
                var mkBackboneModel = backbonizeModel(model, modelName);
                var idHash = {};

                // Backbone and Knockout
                var instance = new mkBackboneModel(args);
                var knockVM = knockBackbone(instance, elName);

                // To let parent instance know about views created for
                // references, we create depViews.
                var depViews = {};

                // Wait for first fetch() of parent model and render references
                //
                // We must render reference views after the model has
                // loaded because the number of refs is unknown when
                // the model has not yet been populated with data.
                //
                // Forms for referenced instances are then rendered
                // with modelSetup which means that viewsWare will be
                // used for further proper cleanup.
                var refCb = function(instance) {
                    // Just once
                    instance.unbind("change", refCb);
                    for (rf in options.refs) {
                        var reference = options.refs[rf];

                        depViews[reference.field] = [];
                        var books = [];
                        books = setupMultiRef(instance,
                                              reference.field,
                                              reference.forest);

                        // Now traverse views that have been generated
                        // for references and setup their models
                        for (rn in books) {
                            var subview = books[rn].refView;
                            depViews[reference.field] =
                                depViews[reference.field].concat(subview);
                            var setup = modelSetup(books[rn].refModelName);
                            setup(subview, {id: books[rn].refId},
                                  {permEl: subview + "-perms",
                                   groupsForest: options.groupsForest,
                                   slotsee: [subview + "-link"]});
                        }
                    }
                }
                instance.bind("change", refCb);

                // External fetch callback
                if (_.isFunction(options.fetchCb)) {
                    instance.bind("change", options.fetchCb);
                }

                var groupViews = {};

                // Render main forms and group field forms
                var content = renderFields(model, elName);
                for (gName in content) {
                    // Main form & permissions
                    if (gName == "_") {
                        $el(elName).html(content[gName]);
                        $el(options.permEl).html(
                            renderPermissions(model, elName));
                    }
                    else
                    {
                        var view =
                            mkSubviewName(gName, 0, instance.name, instance.cid);
                        depViews[gName] = [view];
                        groupViews[gName] = view;
                        // Subforms for groups
                        $el(options.groupsForest).append(
                            renderDep({refField: gName,
                                       refN: 0,
                                       refView: view},
                                      getTemplates("group-template")));
                        // render actual view content in the '.content'
                        // children of view block, so we can add
                        // custom elements to decorate view
                        $el(view).find('.content').html(content[gName]);
                    }
                }

                // Bind the model to Knockout UI
                ko.applyBindings(knockVM, el(elName));
                // Bind group subforms (note that refs are bound
                // separately)
                for (s in groupViews) {
                    ko.applyBindings(knockVM, el(groupViews[s]));
                }
                // Bind extra views if provided
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

                // Bookkeeping
                global.viewsWare[elName] = {
                    "model": model,
                    "bbInstance": instance,
                    "modelName": modelName,
                    "mkBackboneModel": mkBackboneModel,
                    "knockVM": knockVM,
                    "depViews": depViews
                };

                // Run global hooks
                for (f in global.modelHooks["*"])
                    global.modelHooks["*"][f](elName);

                // Run model-specific hooks
                if (_.has(global.modelHooks, modelName))
                    for (f in global.modelHooks[modelName])
                        global.modelHooks[modelName][f](elName);
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
            var parentId = parentInstance.name + ":" + parentInstance.id;
            refInstance.set({"parentId": parentId});
        }
    }
    return fetchCb;
}

// Build name of view for refN-th reference stored in refField of
// instance of model name parentModelName with id parentId.
function mkSubviewName(refField, refN, parentModelName, parentId) {
    return parentModelName + "-" + parentId + "-" +
        refField + "-view-" + refN;
}

// Build name of class of views for references stored in refField of
// parentModelName with given id.
function mkSubviewClass(refField, parentModelName, parentId) {
    return parentModelName + "-" + parentId + "-" +
        refField + "-views";
}

// Render views for references stored in given instance field.
//
// We generate a forest of views in element refsForest where each view
// has id generated by mkSubviewName and class mkSubviewClassSelector.
// renderDep is used to build every view.
//
// Return array with book for every view generated this way. Every
// book is an object with keys refN, refModel, refId and refView.
//
// Example: [{refN: 0,
//            refModel: "towage",
//            refId: "2131",
//            refView: "services-view-0"},..]
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
    $el(refsForest).empty();

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
                           refField: refField,
                           refClass: mkSubviewClass(refField,
                                                    instance.name,
                                                    instance.cid),
                           refView: mkSubviewName(refField, i,
                                                  instance.name,
                                                  instance.cid)
                          };
            books[i] = refBook;

            referenceViews += renderDep(refBook, tpls);
        }
        $el(refsForest).append(referenceViews);
    }
    return books;
}

// Add new reference to instance, render view and setup form for it.
//
// @param instance Backbone model
//
// @param reference is an object which contains:
//
// - field is the name of instance field to store reference in;
// 
// - modelName sets name of model to create reference to;
//
// - forest is ID of element which holds views for references.
//
// @param groupsForest sets name of view to render groups of created
// reference model in.
//
// @param options Custom extra options for modelSetup called for added
// referenced model
// 
// @return Single book of the same structure as in setupMultiRef result
// (but without refId).
function addReference(instance, reference, groupsForest, options) {
    var oldValue = instance.get(reference.field);
    var tpls = getTemplates("reference-template");
    if (_.isEmpty(oldValue))
        oldValue = [];
    else
        oldValue = oldValue.split(",");

    var refClass = mkSubviewClass(reference.field, instance.name, instance.cid);

    // Cannot check against oldService.length because there may be
    // more than 1 unsaved service
    var refN = $("." + refClass).length;

    var refView = mkSubviewName(reference.field, refN, instance.name, instance.cid);

    var book = {refN: refN,
                refModelName: reference.modelName,
                refId: null,
                refField: reference.field,
                refClass: refClass,
                refView: refView,
               };

    // Render view
    var html = renderDep(book, tpls);
    $el(reference.forest).append(html);

    var fetchCb = mkRefFetchCb(instance, reference.field);
    modelSetup(reference.modelName)(
        refView, null,
        _.extend({fetchCb: fetchCb,
                  groupsForest: groupsForest,
                  slotsee: [refView + "-link"],
                  permEl: refView + "-perms"
                 }, options));
    return book;
}

// Generate HTML contents for view which will be populated by
// referenced instance described by keys of refBook:
//
// refN - number of instance in reference field of parent model;
//
// refId - id of model instance being referenced;
//
// refField - name of field of parent model which stores reference;
//
// refView - name of reference view. where instance will be rendered
// after loading.
//
// refClass - class of reference views in this book.
//
// refBook may contain any other keys as well and will be passed to
// Mustache.render as a context.
//
// Templates will be pickTemplate'd against using
// <refModelName>-<refField>, simply <refField> or default template.
//
// Every view template MUST set div with id=<refView> and
// class=<refClass> where model will be setup; an element with
// id=<refView>-link which will be bound to KnockVM of referenced
// instance; possibly <refView>-perms for rendering instance
// permissions template.
//
// This may also be used to render any dependant views for model to
// maintain unique ids.
function renderDep(refBook, templates) {
    var typed_tpl = refBook.refField;
    return Mustache.render(pickTemplate(templates, [typed_tpl, ""]),
                           refBook);
}
