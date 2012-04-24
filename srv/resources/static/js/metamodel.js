/// Transfrom model definitions into Backbone models, render model
/// forms, template helpers.

// Backbonize a model, set default values for model
//
// @return Constructor of Backbone model
function backbonizeModel(model, modelName) {
    var defaults = {};
    var fieldHash = {};
    var dictionaryFields = [];
    var referenceFields = [];
    var requiredFields = [];
    var groups = []
    _.each(model.fields,
          function(f) {
              if (!_.isNull(f.meta)) {
                  if (_.has(f.meta, "required") && f.meta.required)
                      requiredFields = requiredFields.concat(f.name);
                  if (_.has(f.meta, "default"))
                      defaults[f.name] = f.default;
                  else
                      defaults[f.name] = null;
              } else
                  // still add field to model even if meta is not present
                  defaults[f.name] = null;

              fieldHash[f.name] = f;

              if (f.type == "reference")
                  referenceFields = referenceFields.concat(f.name);
              if (f.type == "dictionary")
                  dictionaryFields = dictionaryFields.concat(f.name);
              if (!_.isNull(f.groupName) && (groups.indexOf(f.groupName) == -1))
                  groups = groups.concat(f.groupName);
          });

    var M = Backbone.Model.extend({
        defaults: defaults,

        // Field caches

        // List of fields with dictionary type
        dictionaryFields: dictionaryFields,
        // List of field names which hold references to different
        // models.
        referenceFields: referenceFields,
        // List of required fields
        requiredFields: requiredFields,
        // List of groups present in model
        groups: groups,

        // Temporary storage for attributes queued for sending to
        // server.
        attributeQueue: {},
        initialize: function() {
            if (!this.isNew())
                this.fetch();
        },
        // Original definition
        //
        // This model and Backbone model (which is actually a
        // representation of instance of original model) are not to be
        // confused!
        model: model,
        // Name of model definition
        name: modelName,
        // Readable model title
        title: model.title,
        // Hash of model fields as provided by model definition.
        fieldHash: fieldHash,
        // Bind model changes to server sync
        setupServerSync: function () {
            var realUpdates = function () {
                // Do not resave model when id is set after
                // first POST
                //
                // TODO Still PUT-backs
                if (!this.hasChanged("id"))
                    this.save();
            };

            this.bind("change", _.throttle(realUpdates, 500), this);
        },
        set: function(attrs, options){
            Backbone.Model.prototype.set.call(this, attrs, options);
            // Push new values in attributeQueue
            //
            // Never send "id", never send anything if user has no
            // canUpdate permission.
            //
            // Note that when model is first populated with data from
            // server, all attributes still make it to the queue,
            // resulting in full PUT-back upon first model "change"
            // event.
            //
            // TODO _.extend doesn't work here
            for (k in attrs)
                if (k != "id" &&
                    (_.has(this.fieldHash, k)) &&
                    this.model.canUpdate &&
                    this.fieldHash[k].canWrite &&
                    (!_.isNull(attrs[k])))
                    this.attributeQueue[k] = attrs[k];
        },
        // Do not send empty updates to server
        save: function(attrs, options) {
            if (!_.isEmpty(this.attributeQueue))
                Backbone.Model.prototype.save.call(this, attrs, options);
        },
        // For checkbox fields, translate "0"/"1" to false/true
        // boolean.
        parse: function(json) {
            var m = this.model;
            for (k in json) {
                // TODO Perhaps inform client when unknown field occurs
                if ((k != "id") && _.has(this.fieldHash, k)) {
                    var type = this.fieldHash[k].type;
                    if (type.startsWith("date") && json[k].match(/\d+/)) {
                        var format = type == "date"
                                   ? "dd.MM.yyyy"
                                   : "dd.MM.yyyy HH:mm:ss";
                        json[k] = new Date(json[k] * 1000).toString(format);
                    }
                    else if (type == "checkbox") {
                        json[k] = json[k] == "1";
                    }
                }
            }
            return json;
        },
        toJSON: function () {
            // Send only attributeQueue instead of the whole object
            var json = this.attributeQueue;
            // Map boolean values to string "0"/"1"'s for server
            // compatibility
            for (k in json) {
                if (_.has(this.fieldHash, k) &&
                    this.fieldHash[k].type.startsWith("date"))
                {
                    var date = Date.parseExact(
                            json[k],
                            ["dd.MM.yyyy", "dd.MM.yyyy HH:mm:ss"]);
                    if (date) {
                        var timestamp = Math.round(date.getTime() / 1000);
                        json[k] = String(timestamp);
                    }
                }
                if (_.isBoolean(json[k]))
                    json[k] = String(json[k] ? "1" : "0");
            }
            this.attributeQueue = {};
            return json;
        },
        urlRoot: "/_/" + modelName
    });

    return M;
}

// Get all templates with given class, stripping "-<class>" from id of
// every template.
//
// TODO Cache this
function getTemplates(cls) {
    var templates = {};
    _.each($("." + cls),
           function(tmp) {
               templates[tmp.id.replace("-" + cls, "")] = tmp.text;
           });
    return templates;
}

// Pick a template from cache which matches one of given names first.
function pickTemplate(templates, names) {
    for(i = 0; i < names.length; i++)
        if (_.has(templates, names[i]))
            return templates[names[i]];
    return Mustache.render($("#unknown-template").html(),
                           {names:names});
}

// Convert model to forest of HTML form elements with appropriate
// data-bind parameters for Knockout, sectioning contents wrt to group
// fields.
//
// For every model field, an appropriate form widget is picked using
// chooseFieldTemplate and actual value may be then rendered in field
// using Knockout.
//
// Field templates are rendered with field definitions as context.
// Extra context attributes include:
//
// - `readonly` for fields which have canWrite=false in form
// description, so templates can use it.
//
// - `viewName` which is viewName passed into renderFields.
//
// To allow rendering of elements which depend on the name of view
// which will hold the instance (like save/remove instance), viewName
// argument is passed.
//
// MAYBE: We can do this on server as well.
//
// @return Hash where keys are names of first fields in each group
// (section names) and values are string with HTML for respective
// section (group field). Groupless fields (those which belong to main
// group) are rendered into value stored under "_" key (main section).
//
// First field of group and fields which have meta annotation
// `mainToo` are always put in main section as well.
//
// There's no way to fully include group fields in main section except
// giving `mainToo` annotation in each field of group.
function renderFields(model, viewName) {
    var templates = getTemplates("field-template");

    var contents = {};
    var fType = "";
    var group = "";
    var readonly = false;
    var mainGroup = "_";
    var slices;

    // Currently we store the name of «current group» while traversing
    // all model fields. When this name changes, we consider the
    // previous group closed. A better approach would be to include
    // group information in served model.
    //
    // We rely on the fact that fields of groups have `<groupname>_`
    // name prefix.
    var currentGroup = mainGroup;
    var currentSection = mainGroup;

    contents[mainGroup] = "";

    _.each(model.fields,
           function (f) {
             if (_.isNull(f.meta) || !f.meta.invisible) {
                 if (!_.isNull(f.meta)) {
                     f.readonly = f.meta.readonly;
                 }
                 // Note the difference: `meta.readonly` is
                 // client-only annotation to override standard
                 // permissions. Plain `readonly` is passed to
                 // template context and indicates real permissions
                 // for field (wrt role information).
                 readonly = f.readonly || !model.canUpdate || !f.canWrite;

                 // Add extra context prior to rendering
                 var ctx = {readonly: readonly,
                            viewName: viewName};

                 // If group ended, or group spliced for different
                 // original field started, we'll put contents to
                 // different section.
                 slices = /(\w+)_(\w+)/.exec(f.name);
                 if (!_.isNull(slices))
                     group = slices[1];
                 else
                     group = mainGroup;

                 if (f.meta && _.has(f.meta, "infoText"))
                     f.meta.infoText = global.dictionaries.InfoText[f.meta.infoText];

                 if (f.type == "dictionary")
                     ctx = _.extend(ctx,
                                    {dictionary:
                                     global.dictionaries[f.meta.dictionaryName]});
                 ctx = _.extend(f, ctx);

                 // We temprorarily change field type when rendering
                 // first field of group, so store real type here.
                 var realType = f.type;
                 var tpl;

                 // Put first field in group in main section, too.
                 // Render it as if it had `group` type.
                 if (group != currentGroup) {
                     currentGroup = group;
                     if (currentGroup == mainGroup)
                         currentSection = mainGroup;
                     else {
                         // Due to prefixing this is unique for any
                         // group
                         currentSection = f.name;

                         if (f.meta && (!f.meta.mainOnly)) {
                             f.type = "group";
                             tpl = chooseFieldTemplate(f, templates);
                             contents[mainGroup]
                                 += Mustache.render(tpl, ctx);
                             f.type = realType;
                         }
                     }
                 }

                 // Initialiaze new section contents
                 if (!_.has(contents, currentSection))
                     contents[currentSection] = "";

                 tpl = chooseFieldTemplate(f, templates);

                 // Put field HTML in appropriate section
                 contents[currentSection] += Mustache.render(tpl, ctx);

                 if (f.meta && (f.meta.mainToo || f.meta.mainOnly))
                     contents[mainGroup] += Mustache.render(tpl, ctx);
             }
           });
    return contents;
}

// Pick first template element with id which matches:
// <field.name>-<field.type>, <field.meta.widget>-<field.type>,
// <field.type>
function chooseFieldTemplate(field, templates) {
    var typed_tpl = field.type;
    var named_tpl = field.name + "-" + field.type;
    var widget_tpl = "";
    if ((!_.isNull(field.meta)) && (_.has(field.meta, "widget")))
        widget_tpl = field.meta.widget + "-" + field.type;

    var tpl = pickTemplate(templates,
                           [named_tpl, widget_tpl, typed_tpl, "unknown"]);
    return tpl;
}

// Render permissions controls for form holding an instance in given
// view.
//
// @return String with HTML for form
function renderPermissions(model, viewName) {
    var modelRo = !model.canUpdate && !model.canCreate && !model.canDelete;
    // Add HTML to contents for non-false permissions
    return Mustache.render($("#permission-template").text(),
                           _.extend(model, {viewName: viewName,
                                            readonly: modelRo}));
}
