// read initial value from 'value' attribute

ko.bindingHandlers.initializeValue = {
    init: function(element, valueAccessor) {
        valueAccessor()(element.getAttribute('value'));
    },
    update: function(element, valueAccessor) {
        var value = valueAccessor();
        element.setAttribute('value', ko.utils.unwrapObservable(value))
    }
};

ko.bindingHandlers.option = {
    update: function(element, valueAccessor) {
        var value = ko.utils.unwrapObservable(valueAccessor());
        ko.selectExtensions.writeValue(element, value);   
    }        
};
