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
        
        SEL_CONTAINER = ".tab-content",
        SEL_MENU = "#admin-yaml-import-wizard-menu",
        SEL_FORM = "#admin-yaml-import-form",
        SEL_UPLOAD_BTN = "#admin-yaml-import-upload-btn",
        SEL_BUTTON_NEXT = ".admin-yaml-import-record-btn-next",
        SEL_BUTTON_PREV = ".admin-yaml-import-record-btn-prev",
        SEL_PAGE_STATUS = ".admin-yaml-import-record-pagenum",
        SEL_BUTTON_SUBMIT_IMPORT = ".admin-yaml-import-submit-profile",
        SEL_BUTTON_SUBMIT_TESTRUN = ".admin-yaml-import-submit-testrun",
        SEL_FEEDBACK_PROFILE = ".admin-yaml-import-profile",
        
        CLASS_DISABLED = "admin-yaml-import-record-btn-disabled",
        
        // functions
        initModel,
        updateModel,
        getData,
        initProcessableFormFields,
        processForm,
        clearJson,
        populateJson,
        submitForm,
        requestValues,
        updateValues,
        updateUI,
        initSettingsUI,
        initUI,
        activateStep
        ;
    
    initModel = function(record) {
        if (viewModel) {
            ko.cleanNode($(SEL_CONTAINER)[0]);
            viewModel = null;
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
    
    clearJson = function() {
        $(SEL_FORM).find("[name=json]").val("");
    }
    
    populateJson = function(isTestRun) {
        var data = JSON.parse(viewModel.toJSON());
        data.testrun = isTestRun;
        var jsonData = JSON.stringify(data);
        $(SEL_FORM).find("[name=json]").val(jsonData);
    }
    
    submitForm = function(e, isTestRun) {
        viewModel.results(undefined);
        viewModel.error(undefined);
        populateJson(isTestRun);
        $(SEL_FORM).submit();
        
        // assuming that the data is always correct, proceed to next tab:
        activateStep("results");
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
        $(SEL_FORM).on("change", "input[type=file]", function() {
            clearJson();
            $(SEL_FORM).submit();
        });
        $(SEL_UPLOAD_BTN).on("click", function(e) {
            e.preventDefault();
            e.stopPropagation();
            clearJson();
            $(SEL_FORM).submit();
        });
        activateStep("upload");
        $(SEL_FORM).show();
    }
    
    initSettingsUI = function() {
        $(SEL_BUTTON_SUBMIT_IMPORT).on("click", function(e) {
            e.preventDefault();
            e.stopPropagation();
            submitForm(e, false);
        });
        $(SEL_BUTTON_SUBMIT_TESTRUN).on("click", function(e) {
            e.preventDefault();
            e.stopPropagation();
            submitForm(e, true);
        });
        $(SEL_BUTTON_PREV).on("click", function(e) {
            e.preventDefault();
            e.stopPropagation();
            requestValues(pageNum - 1);
        });
        $(SEL_BUTTON_NEXT).on("click", function(e) {
            e.preventDefault();
            e.stopPropagation();
            requestValues(pageNum + 1);
        });
        $(".admin-yaml-import-save-settings").tooltip();
        pageNum = 1;
        processForm();
    }

    activateStep = function(stepId) {
        $(SEL_MENU + ' a[href="#' + stepId + '"]').tab("show");
    }
    
    return {
        init: function() {
            initUI();
        },
        
        initRecord: function(jsonData) {
            activateStep("settings");
            initModel(JSON.parse(jsonData));
            initSettingsUI();
            requestValues(pageNum);
        },
        
        recordImported: function(jsonData) {
            var data = JSON.parse(jsonData);
            viewModel.results(data.imported);
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