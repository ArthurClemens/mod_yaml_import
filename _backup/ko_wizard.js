var Menu = function() {
    this.sections = [
        {
            name: "upload",
            title: "1. Upload"
        },
        {
            name: "settings",
            title: "2. Settings"
        },
        {
            name: "results",
            title: "3. Results"
        }
    ];
    
    this.selectedSection = ko.observable().subscribeTo("section");
    this.hasData = ko.observable(false).subscribeTo("hasData");
    this.hasResults = ko.observable(false).subscribeTo("hasResults");
    
    this.goSection = function(section) {
        ko.postbox.publish("section", section.name);
    }
};

var BaseView = function (sectionName) {
    var self = this;
    self.name = sectionName;
    self.visible = ko.observable(false);
};

var UploadViewModel = function(sectionName) {
    var self = this;
    ko.utils.extend(self, new BaseView(sectionName));
    ko.postbox.subscribe("section", function(newValue) {
        self.visible(newValue === sectionName);
    }, self);
};

var SettingsViewModel = function(sectionName) {
    var self = this;
    ko.utils.extend(self, new BaseView(sectionName));
    ko.postbox.subscribe("section", function(newValue) {
        self.visible(newValue === sectionName);
    }, self);
//    self.visible = ko.observable(false).syncWith("hasData");
};

var ResultsViewModel = function(sectionName) {
    var self = this;
    ko.utils.extend(self, new BaseView(sectionName));
    ko.postbox.subscribe("section", function(newValue) {
        self.visible(newValue === sectionName);
    }, self);
//    self.visible = ko.observable(false).syncWith("hasResults");
};

var ViewModelFactory = function(sectionName) {
    switch (sectionName) {
        case "upload":
            return UploadViewModel;
        case "settings":
            return SettingsViewModel;
        case "results":
            return ResultsViewModel;
    }
};


jQuery(document).ready(function ($) {
    "use strict";
    
    var menu = new Menu();
    
/*
    var sections = menu.sections;
    for (var i=0; i<sections.length; i++) {
        var section = sections[i],
            name = section.name,
            viewModelRef = ViewModelFactory(name),
            viewModel = new viewModelRef(name);
        ko.applyBindings(viewModel, document.getElementById("pane-" + name));
        section.view = viewModel;
    }
*/
    ko.applyBindings(menu, document.getElementById("admin-yaml-import-wizard-menu"));
    ko.postbox.publish("section", "upload");
});