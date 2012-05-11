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
    "back":
    {
        "template": "back-screen-template",
        "views": {
            "back-form": setupBackOffice
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
        "back": "back",
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
    back: function () {
        renderScreen("back");
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

// jquery -> html(as string) conversion, with selected element
jQuery.fn.outerHTML = function() {
    return jQuery("<div>").append(this.clone()).html();
}

// like _.has but for list
function hasL(lst, e) {
    return _.find(lst, function(x) { return x == e });
}

// render checkboxes, trueChecks contains list with names,
// tha should be rendered as checked
function renderChecks(name, trueChecks) {
    var str = "";
    var tpl = $("#check-list-item-template").html();
    if (_.has(global.checks, name)){
        for (n in global.checks[name]["checks"]){
            var check = global.checks[name]["checks"][n];
            var v = $(Mustache.render(tpl, check));
            if (hasL(trueChecks, check.name)){
                v.find('input:checkbox').attr('checked', true);
            }
            str += v.outerHTML();
        }
    }
    return str;
}

// try to render checkboxes, if check is found, then
// make request to candibober, and render checkboxes
// with 'renderChecks'
function maybeRenderChecks(e, instance){
    var str = "";
    var tpl = $("#check-list-item-template").html();
    var name = instance.get(e.data('depends'));
    if (_.has(global.checks, name)){
        var h = {};
        h[instance.name] = { 'model' : instance.name,
                             'id'    : instance.id
                           };
        $.ajax({ 'dataType' : 'json',
                 'type'     : 'POST',
                 'url'      : '/candibober/check/' + name,
                 'data'     : JSON.stringify(h),
                 'success'  : function(data){
                     e.html(renderChecks(name, data.true));
                 },
                 'error'    : function(){
                     e.html(renderChecks(name, []));
                 }
               });
    }
}

// Update checks information when parent fields change
function candiboberHook(elName) {
    var instance = global.viewsWare[elName].bbInstance;
    $el(elName).find("[data-provide=checklist]").each(
        function(i) {
            (function(e){
                // Grab value of instance field specified in
                // data-depends and render associated checks
                instance.bind("change:" + e.data("depends"),
                              function(v) {
                                  maybeRenderChecks(e, instance);
                              });
            })($(this));
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
            forest: "case-service-references"
        },
        {
            field: "actions",
            forest: "case-actions-references"
        }
    ];

    // Default values
    // FIXME: User's name and creation date are better to be assigned by
    // the server.
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
        
        $("#empty-fields-placeholder").html(
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

    $("body").on("change.input", ".redirectOnChange", function () {
        setTimeout(function() {
          window.location.hash = "back";
        }, 500);
    });
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

function mkDataTable (t) {
    t.dataTable({
        sScrollY: "500px",
        bPaginate: false,
        oLanguage: {
            sSearch: "Фильтр",
            sInfoEmpty: "",
            sZeroRecords: "Ничего не найдено",
            sInfo: "Показаны записи с _START_ по _END_ (всего _TOTAL_)"
      }});
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

    setTimeout(function(){
      $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline";
      $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline";

      var t = $("#partner-table");
      if (t.hasClass("dataTable")) return;
      mkDataTable(t);

      t.on("click.datatable", "tr", function() {
         var id = this.children[0].innerText;
         modelSetup("partner")(
             viewName, {"id": id},
             {permEl: "partner-permissions",
              focusClass: "focusable",
              refs: refs});
      });

      $.getJSON(modelMethod(
            "partner",
            "search?q=*&_limit=1000&_fields=id,name,city,comment"),
          function(rows) {
              var dt = t.dataTable();
              dt.fnClearTable();
              dt.fnAddData(rows);
          });
    }, 100);
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


function setupBackOffice () {
    setTimeout(function() {
        var groupTable = $("#back-group-table");
        mkDataTable(groupTable);
        groupTable.on("click.datatable", "tr", function() {
           var id = this.children[0].innerText.split('/');
           $.ajax({
              type: "PUT",
              url: "/_/action/"+ id[1],
              contentType: "application/json",
              data: '{"assignedTo": "backuser"}',
              processData: false
           });
           window.location.hash = "case/" + id[0];
        });
        var userTable  = $("#back-user-table");
        mkDataTable(userTable);
        userTable.on("click.datatable", "tr", function() {
           var id = this.children[0].innerText.split('/');
           window.location.hash = "case/" + id[0];
        });

        $.getJSON(modelMethod("action", "search?q=*&_limit=1000"),
          function(objs) {
              var ut = userTable.dataTable();
              ut.fnClearTable();
              var gt = groupTable.dataTable();
              gt.fnClearTable();
              for (i in objs) {
                var obj = objs[i];
                
                var id = obj.caseId.replace(/\D/g,'') + "/" + obj.id;
                var duedate = obj.duedate
                         ? new Date(obj.duedate * 1000)
                                .toString("dd.MM.yyyy HH:mm:ss")
                         : '';
                
                var row = [id
                          ,obj.priority || '3'
                          ,duedate
                          ,obj.description || ''
                          ,obj.comment || '']

                if (_.has(obj, 'assignedTo')) {
                  ut.fnAddData(row);
                } else {
                  gt.fnAddData(row);
                }
              }
          });
    },200);
}

 
function removeVinAlert(val) {
    $.post("/vin/state", { id: val } );
}


// FIXME: This could be a callback for main.js:saveInstance
function successfulSave () {
  var $span = $(this).siblings(".save-result")
  setTimeout(function () { 
    $span.text("Сохранено успешно")
    $span.show()
    $span.fadeOut(2000)
  }, 500);
}
