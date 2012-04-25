/// Everything local to the customer resides here

var localScreens = {
    "case":
    {
        "template": "case-screen-template",
        "views":
        {
            "case-form": setupCaseMain
        }
    },
    "search":
    {
        "template": "search-screen-template",
        "views":
        {
            "tableView": setupSearchTable
        }
    },
    "vin":
    {
        "template": "vin-screen-template",
        "views":
        {
            "vin-form": setupVinForm
        }
    },
    "call":
    {
        "template": "call-screen-template",
        "views":
        {
            "call-form": setupCallForm
        }
    },
    "partner":
    {
        "template": "partner-screen-template",
        "views":
        {
            "partner-form": setupPartnersForm
        }
    }
};

// Setup routing
var localRouter = Backbone.Router.extend({
    // Must _not_ end with trailing slashes
    routes: {
        "case/:id": "loadCase",
        "case": "newCase",
        "search": "search",
        "vin": "vin",
        "partner/:id": "loadPartner",
        "partner": "newPartner",
        "call": "call"
    },

    loadCase: function (id) {
        renderScreen("case", {"id": id});
    },

    newCase: function () {
        renderScreen("case", {"id": null});
    },

    search: function () {
        renderScreen("search");
    },

    vin: function () {
        renderScreen("vin");
    },

    newPartner: function () {
        renderScreen("partner", {"id": null});
    },

    loadPartner: function (id) {
        renderScreen("partner", {"id": id});
    },

    call: function () {
        renderScreen("call");
    }
});

var localHooks = {
    "*": [stdElCb, dictionaryHook],
    "case": [candiboberHook]
};

$(function () {
    $.getJSON("/s/js/data/dictionaries.json",
  function(dicts) {
    $.getJSON("/_whoami/",          
  function(user) {
    $.getJSON("/s/js/data/conditions.json",          
  function(checks) {

      mainSetup(localScreens, localRouter, dicts, localHooks, user);
      global.checks = checks;
  });
  });
  });
});

// Clear dependant dictionary fields when parent is changed
function dictionaryHook(elName) {
    var instance = global.viewsWare[elName].bbInstance;
    for (n in instance.dictionaryFields) {
        var fieldName = instance.dictionaryFields[n];
        var parent = instance.fieldHash[fieldName].meta.dictionaryParent;

        if (parent) {
            (function(f){
                instance.bind("change:" + parent,
                              function(v) {
                                  instance.set(f, "");
                              });
            })(fieldName);
        }
    }
}

function renderChecks(name) {
    var str = "";
    var tpl = $("#check-list-item-template").html();
    if (_.has(global.checks, name))
        for (n in global.checks[name]["checks"])
            str += Mustache.render(tpl, global.checks[name]["checks"][n]);
    return str;
}

// Update checks information when parent fields change
function candiboberHook(elName) {
    var instance = global.viewsWare[elName].bbInstance;
    $el(elName).find("[data-provide=checklist]").each(
        function(i) {
            (function(e){
                var d = e.data("depends");
                // Grab value of instance field specified in
                // data-depends and render associated checks
                instance.bind("change:" + e.data("depends"),
                              function(v) {
                                  e.html(renderChecks(instance.get(e.data("depends"))));
                              })})($(this));
        });
}

// Standard element callback which will scroll model into view and
// focus on first field
function stdElCb(elName) {
    var e = $el(elName);
    // Scroll group to the top of the screen
    if (e.hasClass("accordion-inner")) {
        e.parents(".accordion-group")[0].scrollIntoView();
    }
    var f = e.find(".focusable")[0];
    f && f.focus();
};


// Scroll case field into view and focus
function focusField(name) {
    var e = $("#case-form").find("[name=" + name + "]")[0];
    e.scrollIntoView();
    e.focus();
}

// Case view (renders to #left, #center and #right as well)
function setupCaseMain(viewName, args) {
    var refs = [
        {
            field: "services",
            forest: "case-service-references",
        }
    ];

    // Default values
    _.extend(args, {callTaker: global.user.meta.realName,
                    callDate: (new Date).toString ("dd.MM.yyyy HH:mm:ss")});


    // Render list of required fields in right pane
    //
    // bbInstance is available only after model has been loaded. The
    // only way to execute custom code inside modelSetup is using
    // fetchCb option. By the time slotsee's are bound, fetchCb may
    // not have been called yet, thus we explicitly use applyBindings
    // here.
    var fetchCb = function () {
        var instance = global.viewsWare[viewName].bbInstance;
        var ctx = {
            "fields":
            _.map(instance.requiredFields,
                  function (f) {
                      return instance.fieldHash[f];
                  })};
        
        $("#right").html(
            Mustache.render($("#empty-fields-template").html(), ctx));

        ko.applyBindings(global.viewsWare[viewName].knockVM, 
                         el("empty-fields"));
    };

    modelSetup("case")(viewName, args, 
                       {permEl: "case-permissions",
                        focusClass: "focusable",
                        slotsee: ["case-number"],
                        groupsForest: "center",
                        fetchCb: fetchCb,
                        refs:refs});

    // Render service picker
    //
    // We use Bootstrap's glyphs if "icon" key is set in dictionary
    // entry.
    $("#service-picker-container").html(
        Mustache.render($("#service-picker-template").html(),
                        {dictionary: global.dictionaries["Services"]}));

    $(".tableTable").dataTable();
}


// Hide all views on center pane and show view for first reference
// stored in <fieldName> of model loaded into <parentView> there
function showComplex(parentView, fieldName) {
    var depViewName = global.viewsWare[parentView].depViews[fieldName][0];
    var view = $el(depViewName);

    if (view.is(':visible')) return;
    $(".complex-field").hide();

    view.show(function() {
      _.each(view.find(".osMap"), initOSM);
    });
}

// Top-level wrapper for storeService
function addService(name) {
    addReference(global.viewsWare["case-form"].bbInstance,
                 {field: "services",
                  modelName: name,
                  forest: "case-service-references"},
                 "center"
                );
}

function setupCallForm(viewName, args) {
    modelSetup("call")(viewName, null,
                       {permEl: "case-permissions",
                        focusClass: "focusable"});
}

function initOSM(el) {
      if (el.className.contains("olMap")) return;

      var osmap = new OpenLayers.Map(el.id);
      osmap.addLayer(new OpenLayers.Layer.OSM());
      osmap.setCenter(
        new OpenLayers.LonLat(37.617874,55.757549)
          .transform( // from WGS 1984 to Spherical Mercator Projection
            new OpenLayers.Projection("EPSG:4326"),
            new OpenLayers.Projection("EPSG:900913")
          ),
        16 // Zoom level
      );
}

// Dispatch on some picker type
//
// Available picks:
//
// - vinFiller
function doPick(pickType, args) {
    var pickers = {
        
        // Get car_vin field from case and try to fill some of its fields
        // with data stored under this VIN in database.
        vinFiller: function() {
            // How vin fields map to case fields
            var vinMap = 
                {
                    "program": "program",
                    "make": "car_make",
                    "model": "car_model",
                    "plateNumber": "car_plateNum",
                    "mileageTO": "car_checkupMileage",
                    "serviceInterval": "car_checkPeriod"
                };

            var bb = global.viewsWare["case-form"].bbInstance;
            var vin = bb.get('car_vin');

            var vinGroup = $("[name=car_vin]").closest(".control-group");

            $.ajax("/_/vin/" + vin,
                   { 
                       error: function () { 
                           vinGroup.removeClass("success");
                           vinGroup.addClass("error");
                       },

                       success: function (data) {
                           vinGroup.removeClass("error");
                           vinGroup.addClass("success");
                           for (k in vinMap) {
                               if (!_.isUndefined(data[k]))
                                   bb.set(vinMap[k], data[k]);
                           }
                       }
                   });
        },

        callPlease: function(modelName) {
            var bb = global.viewsWare["case-form"].bbInstance;
            var phoneNumber = bb.get(modelName);
            alert ("Calling " + phoneNumber);
        }
    };

    pickers[pickType](args);
}

function setupVinForm(viewName, args) {
    $el(viewName).html($el("vin-form-template").html());
    global.viewsWare[viewName] = {};

    setInterval(getVinAlerts, 5000);
}

function getVinAlerts () {
    $.getJSON("/vin/state", null, function (data) {
	$("#vin-alert-container").html(
	    Mustache.render($("#vin-alert-template").html(), data));
    });
}

function setupPartnersForm(viewName, args) {
    var refs = [
        {
            field: "services",
            forest: "partner-service-references",
        }
    ];
    modelSetup("partner")(
       viewName, args,
       {permEl: "partner-permissions",
        focusClass: "focusable",
        refs: refs});
    $el(viewName).html($el("partner-form-template").html());

    global.viewsWare[viewName] = {};

    $("#partner-service-picker-container").html(
        Mustache.render($("#partner-service-picker-template").html(),
                        {dictionary: global.dictionaries["Services"]}));
}

function addNewServiceToPartner(name)
{
    var instance = global.viewsWare["partner-form"].bbInstance;
    var book = addReference(instance,
                 {field: "services",
                  modelName: "partner_service",
                  forest: "partner-service-references"},
                 "center"
                );
    var service = global.dictionaries.Services;
}

function doVin() {
    var form = $el("vin-import-form")[0];
    var formData = new FormData(form);

    $.ajax({
    	type: "POST",
    	url: "/vin/upload",
    	data: formData,
    	contentType: false,
    	processData: false
    }).done(function( msg ) {
    	alert( "Result: " + msg );
    });
}

function removeVinAlert(val) {
    $.post("/vin/state", { id: val } );
}
