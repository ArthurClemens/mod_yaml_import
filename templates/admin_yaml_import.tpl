{% extends "admin_base.tpl" %}

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
{% endblock %}

