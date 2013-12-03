{% wire id="admin-yaml-import-upload-file" type="submit" postback={upload} delegate="mod_yaml_import" %}
<form class="form" id="admin-yaml-import-upload-file" method="post" action="postback">
    <div class="admin-yaml-import-upload-error"></div>
    <input type="file" name="filename" title="{_ Choose YAML data file _}" />
    <button type="submit" class="btn">{_ Upload _}</button>
</form>