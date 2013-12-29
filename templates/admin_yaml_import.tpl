{% extends "admin_base.tpl" %}
{% block head_extra %}
    {% lib
        "js/libs/bootstrap.file-input.js"
        "js/libs/json2.js"
        "js/libs/underscore.js"

        "js/libs/knockout-3.0.0.js"
        "js/libs/knockout.validation.js"
        "js/libs/knockout.mapping.js"
        
        "js/mod_yaml_import.js"
        "js/ko_fieldModel.js"
        "js/ko_viewModel.js"
        "js/ko_extensions.js"
        "js/ko_bootstrap_modal.js"
    
        "css/mod_yaml_import.css"
    %}
{% endblock %}

{% block content %}
<div class="admin-yaml-import" id="admin-yaml-import">
    <h2>{_ YAML data import _}</h2>
    <noscript><p>{_ You need JavaScript to use the import module. _}</p></noscript>
    <div id="admin-yaml-import-wizard-menu">
        <ul class="nav nav-tabs">
            <li><a href="#upload" data-toggle="tab">1. Upload</a></li>
            <li><a href="#settings" data-toggle="tab">2. Settings</a></li>
            <li><a href="#results" data-toggle="tab">3. Results</a></li>
        </ul>
    </div>
    {% wire id="admin-yaml-import-form" type="submit" postback={import} delegate="mod_yaml_import" %}
    <form class="form" id="admin-yaml-import-form" method="post" action="postback" style="display: none">
        <div class="tab-content">
            <div class="tab-pane" id="upload">
                {% include "_step_upload.tpl" %}
            </div>
            <div class="tab-pane" id="settings">
                {% include "_step_settings.tpl" %}
            </div>
            <div class="tab-pane" id="results">
                {% include "_step_results.tpl" %}
            </div>
        </div>
    </form>    
</div>
{% include "_js_data.tpl" %}
{% javascript %}
mod_yaml_import.init();
{% endjavascript %}
{% endblock %}

