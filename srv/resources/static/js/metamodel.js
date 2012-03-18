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
              if (!(_.isUndefined(f.default)))
                  defaults[f.name] = f.default;
              else
                  defaults[f.name] = null;
              fieldHash[f.name] = f;
              if (f.type == "reference")
                  referenceFields = referenceFields.concat(f.name);
              if (f.type == "dictionary")
                  dictionaryFields = dictionaryFields.concat(f.name);
              if ((!_.isUndefined(f.required)) && f.required)
                  requiredFields = requiredFields.concat(f.name);
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
                    (!_.isUndefined(this.fieldHash[k])) &&
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
                if ((k != "id") && 
                    (!_.isUndefined(this.fieldHash[k])) &&
                    (this.fieldHash[k].type == "checkbox")) {
                    if (json[k] == "1")
                        json[k] = true;
                    else
                        json[k] = false;
                }
            }
            return json;
        },
        toJSON: function () {
            // Send only attributeQueue instead of the whole object
            var json = this.attributeQueue;
            // Map boolean values to string "0"/"1"'s for server
            // compatibility
            for (k in json)
                if (_.isBoolean(json[k]))
                    json[k] = String(json[k] ? "1" : "0");
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
        if (!_.isUndefined(templates[names[i]]))
            return templates[names[i]];
    return Mustache.render($("#unknown-template").html(), 
                           {names:names});
}

// Convert model to forest of HTML form elements with appropriate
// data-bind parameters for Knockout.
//
// To allow rendering of elements which depend on the name of view
// which will hold the instance (like save/remove instance), viewName
// argument is passed.
//
// Groups is a hash where keys are group names and values are views to
// render the respected group in. First field of group is always
// rendered in main view as well.
//
// TODO: We can do this on server as well.
//
// @return Hash where keys are group names (or "_" for main group) and
// values are string with HTML of forms.
function renderFields(model, viewName, groups) {
    var templates = getTemplates("field-template");

    var contents = {};
    var fType = "";
    var group = "";
    var readonly = false;
    // Pick an appropriate form widget for each model
    // field type and render actual model value in it
    //
    // Set readonly context attribute for fields which have
    // canEdit=false in form description.
    _.each(model.fields,
           function (f) {
             if (!f.invisible) {
                 var typed_tpl = f.type;
                 var named_tpl = f.name + "-" + f.type;
                 var tpl = pickTemplate(templates, 
                                        [named_tpl, typed_tpl, "unknown"]);
                 readonly = !model.canUpdate || !f.canWrite;

                 group = f.groupName || "_";

                 // Add extra context prior to rendering
                 var ctx = {readonly: readonly,
                            viewName: viewName,
                            groupName: group};
                 if (f.type == "dictionary")
                     ctx = _.extend(ctx, 
                                    {dictionary: global.dictionaries[f.dictionaryName]});

                 // Initialiaze new group contents
                 if (_.isUndefined(contents[group]))
                     contents[group] = "";

                 contents[group] += Mustache.render(tpl, _.extend(f, ctx));
             }
           });
    return contents;
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
