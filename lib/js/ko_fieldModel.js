/*
Stores field structure:
- field.id is used to retrieve the field value from viewModel.fieldValues
*/
mod_yaml_import.fieldModel = function(fieldStructure) {
    var self = this,
        keys = _.keys(fieldStructure);

    this.id = fieldStructure["name"];

    _.map(keys, function(k) {
        var v = fieldStructure[k];
        if (typeof v === "string" && v === "undefined") {
            v = undefined;
        }
        self[k] = ko.observable(v);
    });
    
    this.typeViewModel = new mod_yaml_import.fieldTypeViewModel(this);
}

mod_yaml_import.fieldModel.prototype.toJS = function() {
    return {
        id: this.id,
        type: this.typeViewModel.type(),
        name: this.name(),
        mapping: this.typeViewModel.mappingData()
    };
};



/*
Field type and mapping.
*/

mod_yaml_import.fieldTypeViewModel = function(parent) {

    var self = this;

    this.nameLabel = ko.observable(parent.name());
    parent.name.subscribe(function(nameValue) {
        self.nameLabel(nameValue);
    });
    
    this.typeOptions = mod_yaml_import.constants.types;
    this.type = ko.observable("text");
        
    this.connectionCategory = ko.observable();
    this.statusMapping = ko.observable();
    this.predicateMapping = ko.observable();
    this.fieldMapping = ko.observable();
    this.makelist = ko.observable(false);
    
    var mappingLabel = function() {
        switch (self.type()) {
            case "connection":
                return "category=" + self.connectionCategory() + ", predicate=" + self.predicateMapping() + ", field=" + self.fieldMapping();
                break;
            case "status":
                return self.statusMapping();
                break;
            default:
                return "";
        };
    };
    
    this.typeLabel = ko.computed(function() {
        var label,
            mappingString;
            
        label = this.type();
        mappingString = mappingLabel();

        if (mappingString) {
            label += ": " + mappingString;
        }
        return label;
    }, this);
    
    this.mappingData = ko.computed(function() {
        switch (this.type()) {
            case "connection":
                return {
                    category: this.connectionCategory(),
                    predicate: this.predicateMapping(),
                    field: this.fieldMapping(),
                    makelist: this.makelist()
                };
                break;
            case "status":
                return {
                    status: this.statusMapping()
                };
                break;
            case "medium":
                return {
                    makelist: this.makelist()
                }
            default:
                return undefined;
        };
    }, this);
};

mod_yaml_import.fieldTypeViewModel.prototype.edit = function() {
    
    var settingsData = ko.mapping.toJS(this);
    var editViewModel = new mod_yaml_import.fieldTypeEditModel(settingsData);
    
    mod_yaml_import.modal.show({
        viewModel: editViewModel,
        context: this // Set context so we don't need to bind the callback function
    }).then(function(updatedSettings) {
        ko.mapping.fromJS(updatedSettings, {}, this);
    });
};


mod_yaml_import.fieldTypeEditModel = function(settings) {

    // Copy settings properties into this object.
    // An observable property is created for each.
    // e.g. this.text = ko.observable(settings.text);
    ko.mapping.fromJS(settings, {}, this);

    var self = this;
    
    this.mappingOptions = ko.observableArray([]);
    this.categoryOptions = ko.observableArray([]);
    this.predicateOptions = ko.observableArray([]);
    this.predicateMapping.subscribe(function(predicateId) {
        if (predicateId !== -1) {
            mod_yaml_import.getCategories(
                function(categories) {
                    self.categoryOptions(categories);
                },
                undefined,
                predicateId
            );
        }
    });
    
    this.type.subscribe(function(typeValue) {
        self._setMappingOptions(typeValue);
    });

    // update with existing values
    self._setMappingOptions(this.type());
    self.predicateMapping(this.predicateMapping());
    self.predicateMapping.valueHasMutated();
};

mod_yaml_import.fieldTypeEditModel.prototype.template = "admin-yaml-import-type-dialog-template";

mod_yaml_import.fieldTypeEditModel.prototype.update = function () {
    // Convert this view model's observable properties into a plain JavaScript object. 
    // Then pass that back to the calling code.
    var updatedSettings = ko.mapping.toJS(this);
    
    // delete edit-model-only variables
    delete updatedSettings.mappingOptions;
    delete updatedSettings.categoryOptions;
    delete updatedSettings.predicateOptions;
    
    // delete computed variables
    delete updatedSettings.typeLabel;
    delete updatedSettings.mappingData;
    delete updatedSettings.mappingLabel;
    
    this.modal.close(updatedSettings);
};

mod_yaml_import.fieldTypeEditModel.prototype.cancel = function () {
    // Close the modal without passing any result data.
    this.modal.close();
};

mod_yaml_import.fieldTypeEditModel.prototype._setMappingOptions = function (typeValue) {
    var type,
        self = this;

    _.map(mod_yaml_import.constants.types, function(o) {
        if (o.id === typeValue) {
            type = o;
        }
    });
    
    if (type && type.id === "connection") {
        mod_yaml_import.getPredicates(function(predicates) {
            self.predicateOptions(predicates);
        }, undefined);
    }
    
    if (type && type.mapping) {
        this.mappingOptions(type.mapping);
    }
};
