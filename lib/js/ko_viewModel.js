mod_yaml_import.viewModel = function(record, predicates) {

    var self = this;
    
    var CategoryOptionGroup = function(label, children) {
        this.label = ko.observable(label);
        this.children = ko.observableArray(children);
    };

    // find the 'connection' in the data structure
    var connectionType = _.find(mod_yaml_import.constants.types, function(t) {
        return t.id === "connection";
    });
    // found, now add the predicates.
    if (connectionType) {
        connectionType.mapping = predicates;
    }
    
    this.subscriptions = []; // will notify subscribers
    
    // observables
    this.fieldValues = ko.observable();

    this.selectedFields = ko.observableArray([]);
    this.fields = ko.observableArray(_.map(record, function(fieldStructure) {
        var field = new mod_yaml_import.fieldModel(fieldStructure);
        self.subscriptions.push(field.typeViewModel.type);
        self.subscriptions.push(field.typeViewModel.mappingData);
        self.subscriptions.push(field.name);
        return field;
    }));

    this.titleField = ko.observable("title");
    
    this.categories = ko.observableArray([]);
    this.metaCategories = ko.observableArray([]);
    this.selectedCategory = ko.observable();
    this.categoryGroups = ko.observableArray([]);
    
    var createCategoryGroups = function() {
        self.categoryGroups([
            new CategoryOptionGroup("Categories", self.categories()),
            new CategoryOptionGroup("Meta categories", self.metaCategories())
        ]);
        // default
        self.selectedCategory(self.categoryGroups()[0]);
    };
    
    mod_yaml_import.getCategories(function(categories) {
        self.categories(categories);
        if (self.categories().length && self.metaCategories().length) {
            createCategoryGroups();
        }
    }, undefined);
    
    mod_yaml_import.getMetaCategories(function(metaCategories) {
        self.metaCategories(metaCategories);
        if (self.categories().length && self.metaCategories().length) {
            createCategoryGroups();
        }
    }, undefined);
    
    this.rangeFrom = ko.observable("1"); // string       
    this.rangeTo = ko.observable(); // string
     
    this.results = ko.observableArray();
    this.error = ko.observable();
    
    // notify of changes
    
    this.subscriptions.push(this.titleField);
    this.subscriptions.push(this.selectedFields);
    this.subscriptions.push(this.selectedCategory);
    this.subscriptions.push(this.rangeFrom);
    this.subscriptions.push(this.rangeTo);

     // utility functions
    
    this.getFieldAttr = function (id, attr) {
        var field = _.find(self.fields(), function(f) {
            return f.id === id;
        });
        return field[attr]();
    }
    
    /*
    Parses a list of Key value objects into one object.
    */
    this.setFieldValues = function(values) {         
        var keyValues = {};
        _.map(values, function(o) {
            return _.map(_.keys(o), function (k) {
                var v = o[k];
                if (_.isArray(v)) {
                    keyValues[k] = v.join(", ");
                } else {
                    keyValues[k] = v;
                }
            });
        });
        self.fieldValues(keyValues);
    }
    
    this.fieldValue = function (field) {
        if (self.fieldValues()) {
            return self.fieldValues()[field.id];
        }
    }
    
    this.includeChecked = function(field) {
        if (field.included()) {
            self.selectedFields.push(field);
        } else {
            self.selectedFields.remove(field);
        }
    }
        
    this.isTitle = function(field) {
        return (field.id === "title");
    }
    
    this.noTitle = function() {
        var hasTitle = _.find(self.fields(), function(f){ return f.id === "title"; });
        return !hasTitle;
    }

    this.toJSON = function() {
        var o = {
            category: self.selectedCategory(),
            fields: _.map(self.selectedFields(), function(f) {
                return f.toJS();
            }),
            titleField: self.titleField(),
            rangeFrom: self.rangeFrom(),
            rangeTo: self.rangeTo()
        }
        return JSON.stringify(o);
    }
    
    // validation
    ko.validation.rules["validateAsync"] = {
        async: true,
        validator: function(val, attrs, callback) {
            // val will be the value of the viewmodel's property() observable
            // field is the field name for this rule
            // callback is what you need to tell ko.validation about the result

            if (val === undefined) {
                return;
            } 
            var settings = attrs.data;
            if (settings === undefined) {
                return;
            }
            if (settings.name === undefined) {
                return;
            }
            if (settings.onlyIf) {
                if (settings.onlyIf() === false) {
                    callback({isValid: true, message: ""});            
                    return;
                }
            }
            var d = {
                field: settings.name,
                value: val
            };
            $.ajax({
                url: mod_yaml_import.constants.urls.URL_VALIDATE,
                type: "post",
                dataType: "json",
                data: {json: JSON.stringify(d)},
            })
            .done(function(data, textStatus, jqXHR) {
                var result = data.result;
                if (result === 1) {
                    callback({isValid: true, message: ""});            
                } else {
                    callback({isValid: false, message: result});
                }
            })
            .fail(function(jqXHR, textStatus, errorThrown) {
                //
            });
        }
    };
    ko.validation.registerExtenders();

    this.rangeFrom.extend({
        validateAsync: {
            data: {
                name: "rangeFrom"
            }
        }
    });
    this.rangeTo.extend({
        validateAsync: {
            data: {
                name: "rangeTo"
            }
        }
    });
    _.map(this.fields(), function(field) {
        field.name.extend({
            validateAsync: {
                data: {
                    name: "fieldName",
                    onlyIf: function() {
                        return field.included() && !self.isTitle(field);
                    }
                }
            }
        });
        field.name.valueHasMutated();
    });
};  