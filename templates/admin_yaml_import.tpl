{% extends "admin_base.tpl" %}
{% block head_extra %}
    {% lib
        "js/libs/bootstrap.file-input.js"
        "js/libs/jquery.bootstrap.wizard.js"
        "js/libs/json2.js"
        "js/libs/underscore.js"

        "js/libs/knockout-2.3.0.js"
        "js/libs/knockout.validation.js"
        "js/libs/knockout.mapping-latest.js"
    
        "js/mod_yaml_import.js"
        "js/ko_fieldModel.js"
        "js/ko_viewModel.js"
        "js/ko_extensions.js"
        "js/ko_bootstrap_modal.js"
    
        "css/mod_yaml_import.min.css"
    %}
{% endblock %}

{% block content %}
<div class="admin-yaml-import">
    <h2>{_ YAML data import _}</h2>

    <div id="admin-yaml-import-wizard">
        <ul class="nav">
            <li><a href="#tab1" data-toggle="tab">{_ 1. Data source _}</a></li>
            <li><a href="#tab2" data-toggle="tab">{_ 2. Settings _}</a></li>
            <li><a href="#tab3" data-toggle="tab">{_ 3. Results _}</a></li>
        </ul>
        <div class="tab-content">
            <div class="tab-pane active" id="tab1">
                {% include "_step_upload.tpl" %}
            </div>
            <div class="tab-pane" id="tab2">
                {% include "_step_settings.tpl" %}
            </div>
            <div class="tab-pane" id="tab3">
                {% include "_step_results.tpl" %}
            </div>
        </div>
    </div>
</div>
{% include "_js_data.tpl" %}
{% javascript %}
mod_yaml_import.init();
{% endjavascript %}
{% endblock %}

