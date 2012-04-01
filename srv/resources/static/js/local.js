/// Everything local to the customer resides here

localScreens = {
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
    "call":
    {
        "template": "call-screen-template",
        "views":
        {
            "call-form": setupCallForm
        }
    }
};

// Setup routing
localRouter = Backbone.Router.extend({
    // Must _not_ end with trailing slashes
    routes: {
        "case/:id": "loadCase",
        "case": "newCase",
        "search": "search",
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

    call: function () {
        renderScreen("call");
    }
});

$(function () {
    $.getJSON("/s/js/data/dictionaries.json",
          function(dicts) {
              mainSetup(localScreens, localRouter, dicts);
          });
});


// Standard element callback which will scroll model into view and
// focus on first field
function stdElCb(e) {
    // Scroll group to the top of the screen
    if (e.hasClass("accordion-inner")) {
        e.parents(".accordion-group")[0].scrollIntoView();
    }
    e.find(".focusable")[0].focus();
};


// Case view (renders to #left, #center and #right as well)
function setupCaseMain(viewName, args) {
    var refs = [
        {
            field: "services",
            forest: "case-service-references",
        }
    ];

    // Default values
    _.extend(args, {callTaker: $("#realName").text(),
                    callDate: getFormatDate(),
                    callTime: getFormatTime()});


    // Render list of required fields in right pane
    //
    // bbInstance is available only after model has been loaded. The
    // only way to execute custom code inside modelSetup is using
    // fetchCb option. By the time slotsee's are bound, fetchCb may
    // not have been called yet, thus we explicitly use applyBindings
    // here.
    var fetchCb = function () {
        var ctx = {
            "fields":
            _.map(global.viewsWare["case-form"].bbInstance.requiredFields,
                  function (f) {
                      return global.viewsWare["case-form"].bbInstance.fieldHash[f];
                  })};
        
        $("#right").html(
            Mustache.render($("#empty-fields-template").html(), ctx));

        ko.applyBindings(global.viewsWare["case-form"].knockVM, 
                         el("empty-fields"));
    };

    modelSetup("case")(viewName, args, 
                       {permEl: "case-permissions",
                        focusClass: "focusable",
                        slotsee: ["case-number"],
                        groupsForest: "center",
                        fetchCb: fetchCb,
                        elCb: stdElCb,
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

// Return MM-DD-YYYY
function getFormatDate() {
    var d = new Date;
    var sd = d.getDate() + '.' + (d.getMonth() + 1) + '.' + d.getFullYear();
    return sd;
}

// Return HH:MM
function getFormatTime() {
    var d = new Date;
    var sd = d.getHours() + ':' + d.getMinutes();
    return sd;
}


// Scroll to the bottom of the page
function scrollDown() {
    window.scrollTo(0, document.body.scrollHeight - 300);
}

// Hide all views on center pane and show view for first reference
// stored in <fieldName> of model loaded into <parentView> there
function showComplex(parentView, fieldName) {
    var refViewName = global.viewsWare[parentView].refViews[fieldName][0];
    var view = $el(refViewName);

    if (view.is(':visible')) return;
    $(".complex-field").hide();

    view.show(function() {
      _.each(view.find(".osMap"), initOSM);
    });
}

// Return name field of refInstance the insight observable of parent
// instance must evaluate to.
function insightField(refInstance) {
    switch (refInstance.name) {
        case "caller": {
            return "callerName";
            break;
        }
        case "car": {
            return "vin";
            break;
        }
    }
    return "_none_";
}

// Top-level wrapper for storeService
function addService(name) {
    addReference(global.viewsWare["case-form"].bbInstance,
                 {field: "services",
                  modelName: name,
                  forest: "case-service-references"},
                 "center",
                 {elCb: stdElCb});
    // FIXME (doesn't scroll properly)
    scrollDown();
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

