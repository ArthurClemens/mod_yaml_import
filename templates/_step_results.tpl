<script type="text/html" id="admin-yaml-import-error-template">
    <div class="alert alert-error" data-bind="text: error"></div>
</script>

<script type="text/html" id="admin-yaml-import-results-template">
    <table class="table admin-yaml-results-table">
        <thead>
            <tr>
                <th>{_ Created Pages _}</th>
                <th>{_ Page Connections _}</th>
            </tr>
        </thead>
        <tbody data-bind="foreach: results">
            <tr>
                <td>
                    <a data-bind="attr: {href: pagedata.edit_url}, text: pagedata.title" target="_blank"></a>
                </td>
                <td>
                    <ul data-bind="foreach: connectiondata">
                        <li>
                            <span class="admin-yaml-results-category" data-bind="text: category"></span> <a data-bind="attr: {href: edit_url}, text: title" target="_blank"></a>
                        </li>
                     </ul>
                </td>   
            </tr>
        </tbody>
    </table>
</script>

<div data-bind="template: { name: 'admin-yaml-import-error-template', if: error }"></div>
<div data-bind="template: { name: 'admin-yaml-import-results-template', if: results }"></div>