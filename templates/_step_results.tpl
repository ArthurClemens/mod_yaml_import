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
                    <span data-bind="if: pagedata.edit_url">
                        <a data-bind="attr: {href: pagedata.edit_url}, text: pagedata.title" target="_blank"></a>
                    </span>
                    <span data-bind="ifnot: pagedata.edit_url"><span data-bind="text:pagedata.title"></span>
                    </span>
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