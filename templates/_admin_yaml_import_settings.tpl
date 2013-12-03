<div class="form-horizontal">
    <fieldset>
        <p>{_ Number of records in data source: _} <span class="admin-yaml-import-record-total">{{ pageCount }}</span></p>
    </fieldset>
    
    <fieldset>
        <legend>{_ Category _}</legend>
        <div class="control-group">
            <label class="control-label">{_ Create Page type: _}</label>
            <div class="controls">
                <select data-bind="foreach: categoryGroups, value: selectedCategory">
                    <optgroup data-bind="attr: {label: label}, foreach: children">
                        <option data-bind="text: title, option: id"></option>
                    </optgroup>
                </select>
            </div>
        </div>
    </fieldset>

    <fieldset data-bind="if: noTitle()">
        <legend>{_ Map title field _}</legend>
        <p>{_ No <code>title</code> identifier exists in the data. Select the identifier to copy the value from. _}</p>
        <div class="control-group">
            <label class="control-label">{_ Copy from: _}</label>
            <div class="controls">
                <select data-bind="options: fields, optionsText: 'name', optionsValue: 'id', value: titleField"></select>
            </div>
        </div>
    </fieldset>

    <fieldset>
        <legend>{_ Field settings _}</legend>
        <table class="table admin-yaml-import-table"> 
            <thead>
                <tr>
                    <th class="admin-yaml-import-table-select"></th>
                    <th>{_ Identifier _}</th>
                    <th class="admin-yaml-import-value">
                        <div class="admin-yaml-import-pager">
                            {_ Record _} <span class="admin-yaml-import-record-pagenum"></span>
                            <a href="#" class="admin-yaml-import-record-btn-prev">prev</a>
                            <a href="#" class="admin-yaml-import-record-btn-next">next</a>
                        </div>
                        {_ Value _}
                    </th>
                    <th>{_ Data type _}</th>
                </tr>
            </thead>
            <tbody data-bind="foreach: fields">
                <tr>
                    <!-- select -->
                    <td class="admin-yaml-import-table-select">
                        <input type="checkbox" data-bind="checked: included, event: {change: $parent.includeChecked($data)}, disable: $parent.isTitle($data)" />
                    </td>
                    <!-- name -->
                    <td>
                        <div class="admin-yaml-import-validated">
                            <input type="text" data-bind="value: name, disable: $parent.isTitle($data), valueUpdate: ['afterkeydown','propertychange','input']" />
                        </div>
                    </td>
                    <!-- value -->
                    <td class="admin-yaml-import-value">
                        <span data-bind="text: $parent.fieldValue($data)"> </span>
                    </td>
                    <!-- data type -->
                    <td data-bind="with: typeViewModel">
                        <input type="text" class="admin-yaml-import-mapping" data-toggle="modal" data-bind="value: typeLabel, click: edit" readonly="readonly" />
                    </td>
                </tr>
            </tbody>
        </table>
    </fieldset>

    <fieldset>
        <legend>{_ Range _}</legend>
        <div class="control-group">
            <label class="control-label">{_ Import records _}</label>
            <div class="controls">
                <div class="admin-yaml-import-validated">
                    <input type="text" id="inputRangeFrom" name="inputRangeFrom" class="input-small" data-bind="value: rangeFrom, valueUpdate: ['afterkeydown','propertychange','input']" />
                </div>
                {_ up to _}
                <div class="admin-yaml-import-validated">
                    <input type="text" id="inputRangeTo" name="inputRangeTo" class="input-small" data-bind="initializeValue:rangeTo, value: rangeTo, valueUpdate: ['afterkeydown','propertychange','input']" value="{{ pageCount }}" />
                </div>
            </div>
        </div>
    </fieldset>

    <fieldset>
        <div class="admin-yaml-import-feedback"></div>
        <button class="btn btn-primary admin-yaml-import-submit-profile">{_ Import _}</button>
    </fieldset>

    <textarea name="admin-yaml-import-profile" rows="5" class="admin-yaml-import-profile"></textarea>
</div>