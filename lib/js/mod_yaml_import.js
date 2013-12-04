var mod_yaml_import = (function ($) {
    "use strict";
    
    var viewModel,
        
        // record view
        pageNum,
        hasNextPage,
        hasPreviousPage,
        
        // static
        URL_RECORD_DATA = "/api/yaml_import/view_record",
        URL_SUBMIT = "/api/yaml_import/import",
        
        SEL_CONTAINER = ".admin-yaml-import",
        SEL_BUTTON_NEXT = ".admin-yaml-import-record-btn-next",
        SEL_BUTTON_PREV = ".admin-yaml-import-record-btn-prev",
        SEL_PAGE_STATUS = ".admin-yaml-import-record-pagenum",
        SEL_BUTTON_SUBMIT = ".admin-yaml-import-submit-profile",
        SEL_FEEDBACK_PROFILE = ".admin-yaml-import-profile",
        
        CLASS_DISABLED = "admin-yaml-import-record-btn-disabled",
        SEL_WIZARD = "#admin-yaml-import-wizard",
        
        // functions
        initModel,
        updateModel,
        getData,
        initProcessableFormFields,
        processForm,
        submitForm,
        requestValues,
        updateValues,
        updateUI,
        initUI,
        initWizardUI,
        wizardTabState,
        wizardTabStep
        ;
    
    initModel = function(record) {
        if (viewModel) {
            ko.cleanNode($(SEL_CONTAINER)[0]);
            viewModel = undefined;
        }
        viewModel = new mod_yaml_import.viewModel(record);
        ko.applyBindings(viewModel, $(SEL_CONTAINER)[0]);
        initProcessableFormFields();
    }
    
    /*
    param values: list of key-value pairs
    */
    updateModel = function(values) {
        viewModel.setFieldValues(values);
    }
    
    /*
    Load category list or predicate list.
    */
    getData = function(commandName, callbackOk, callbackError, filter) {
        var command = {
            type: commandName
        };
        if (filter !== undefined) {
            command.filter = filter;
        }
        $.ajax({
            url: mod_yaml_import.constants.urls.URL_GETDATA,
            type: "post",
            dataType: "json",
            data: {json: JSON.stringify(command)}
        })
        .done(function(data, textStatus, jqXHR) {
            var items = _.map(data.result, function(c) {
                return {
                    id: c[0],
                    title:c[1]
                }
            });
            callbackOk(items)
        })
        .fail(function(jqXHR, textStatus, errorThrown) {
            if (callbackError) {
                callbackError(jqXHR, textStatus, errorThrown)
            }
        });
    }

    /*
    Subscribe to viewModel changes so that we can get a good view of the sendable data.
    */
    initProcessableFormFields = function() {
        if (viewModel.subscriptions) {
            _.map(viewModel.subscriptions, function(observable) {
                observable.subscribe(processForm);
            });
        }
    }
    
    processForm = function() {
        var queryJson = viewModel.toJSON();
        if (!$.isEmptyObject(queryJson)) {
            $(SEL_FEEDBACK_PROFILE).val(queryJson);
        }
    }
    
    submitForm = function(e) {
        viewModel.results(undefined);
        viewModel.error(undefined);
        wizardTabStep(3);
        
        $.ajax({
            type: "post",
            url: URL_SUBMIT,
            data: {json: viewModel.toJSON()},
            dataType: "json",
            success: function(data, status) {
                viewModel.results(data.results);
            },
            error: function(xmlHttpRequest, textStatus, errorThrown) {
                viewModel.error("Error: " + errorThrown);
            }
        });
        return false;
    }
    
    /*
    Loads field values async.
    */
    requestValues = function(pageNum) {
        $.ajax({
            type: "post",
            url: URL_RECORD_DATA,
            data: {pageNum: pageNum},
            success: function(data, status) {
                updateValues(data);
            },
            error: function(xmlHttpRequest, textStatus, errorThrown) {
                alert("Could not get field data: " + textStatus + "; " + xmlHttpRequest.responseText);
            }
        });
    }
    
    updateValues = function(data) {
        updateModel(data.values);
        // set UI vars
        pageNum = data.pageNum;
        hasNextPage = data.hasNext;
        hasPreviousPage = data.hasPrevious;
        updateUI();
    }
    
    updateUI = function() {
        $(SEL_PAGE_STATUS).text(pageNum);
        if (hasNextPage) {
            $(SEL_BUTTON_NEXT).removeClass(CLASS_DISABLED);
        } else {
            $(SEL_BUTTON_NEXT).addClass(CLASS_DISABLED);
        }
        if (hasPreviousPage) {
            $(SEL_BUTTON_PREV).removeClass(CLASS_DISABLED);
        } else {
            $(SEL_BUTTON_PREV).addClass(CLASS_DISABLED);
        }
    }
    
    initUI = function() {
        $(SEL_BUTTON_SUBMIT).on("click", submitForm);
        $(SEL_BUTTON_PREV).on("click", function(e) {
            e.preventDefault();
            requestValues(pageNum - 1);
            return false;
        });
        $(SEL_BUTTON_NEXT).on("click", function(e) {
            e.preventDefault();
            requestValues(pageNum + 1);
            return false;
        });
        $(".admin-yaml-import-save-settings").tooltip();
        pageNum = 1;
        processForm();
    }
    
    initWizardUI = function() {
        $(SEL_WIZARD).bootstrapWizard({'tabClass': 'nav nav-tabs'});
        
        wizardTabState(2, "disable");
        wizardTabState(3, "disable");

        $(SEL_WIZARD).bootstrapWizard().on("onTabChange", function(e, data) {
			if (viewModel) {
    			$("li[data-target='#step1']", $(this)).addClass("complete").find(".badge").addClass("badge-success");
    			$("li[data-target='#step2']", $(this)).addClass("complete").find(".badge").addClass("badge-success");
    		}
		});
    }
    
    wizardTabState = function(num, state) {
        var index = num - 1;
        if (state === "disable") {
            $(SEL_WIZARD)
              .find("li:eq(" + index + ")")
              .addClass("disabled")
              .find("a")
              .removeAttr("data-toggle")
              .on("click", function(e) {
                e.preventDefault(e);
              })
              ;
        } else {
            $(SEL_WIZARD)
              .find("li:eq(" + index + ")")
              .removeClass("disabled")
              .find("a")
              .attr("data-toggle", "tab")
              ;
        }
    }
    
    wizardTabStep = function(num) {
        wizardTabState(num, "enable");
        $(SEL_WIZARD).bootstrapWizard("next");
    }
    
    return {
        init: function() {
            initWizardUI();
        },
        
        initRecord: function(jsonData) {            
            wizardTabStep(2);
            initModel(JSON.parse(jsonData));
            initUI();
            requestValues(pageNum);
        },
                
        getCategories: function(callbackOk, callbackError, predicateId) {
            return getData("categories", callbackOk, callbackError, predicateId);
        },
        
        getMetaCategories: function(callbackOk, callbackError) {
            return getData("meta_categories", callbackOk, callbackError);
        },
        
        getPredicates: function(callbackOk, callbackError) {
            return getData("predicates", callbackOk, callbackError);
        }
    };
    
})(jQuery);