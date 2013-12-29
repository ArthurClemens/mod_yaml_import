<div class="modal admin-yaml-import-type-modal">
    <div class="modal-header">
        <button type="button" class="close" aria-hidden="true" data-bind="click: cancel">&times;</button>
        <h3>{_ Set the data type of field: _} <span data-bind="text: nameLabel()"></span></h3>
    </div>
    <div class="modal-body">
        <div class="row-fluid">
            <form action="#" data-bind="submit: update" class="form-horizontal">
                <div class="span4 admin-yaml-import-type-selection">
                    <h4>{_ Select data type _}</h4>
                    <div data-bind="foreach: typeOptions">
                        <label class="radio" data-bind="css: {'admin-yaml-import-type-selected': id() === $parent.type()}">
                            <input type="radio" name="optionsRadios" data-bind="attr: {id: id, value: id}, checked: $parent.type" />
                            <span data-bind="text: title"></span>
                        </label>  
                    </div>
                </div>
                <div class="span8">
                    <div data-bind="foreach: typeOptions">
                        <!-- ko if: $parent.type() === id() -->
                            <div class="admin-yaml-import-type-info">
                                <h4 data-bind="text: title"></h4>
                                <p data-bind="text: info"></p>
                                <hr />
                            </div>
                        <!-- /ko -->
                    </div>
                    <!-- ko if: type() === 'status' -->
                        <select data-bind="options: mappingOptions, optionsText: 'title', optionsValue: 'id', value: $root.statusMapping"></select>
                    <!-- /ko -->
                    <!-- ko if: type() === 'medium' -->
                        <label class="checkbox">
                            <input type="checkbox" data-bind="checked: makelist"> {_ Multiple import from comma-separated list _}
                        </label>
                    <!-- /ko -->
                    <!-- ko if: type() === 'connection' -->
                        <div class="control-group">
                            <label class="control-label">{_ Connect with predicate _}</label>
                            <div class="controls">
                                <!-- ko if: predicateOptions().length > 0 -->
                                    <select data-bind="options: predicateOptions, optionsCaption: '{_ Select ... _}', optionsText: 'title', optionsValue: 'id', value: $root.predicateMapping"></select>
                                <!-- /ko -->
                            </div>
                        </div>
                        <!-- ko if: $root.predicateMapping() !== undefined && categoryOptions().length > 0 -->
                            <div class="control-group">
                                <label class="control-label">{_ To page category _}</label>
                                <div class="controls">
                                    <!-- ko if: categoryOptions().length > 0 -->
                                        <select data-bind="options: categoryOptions, optionsCaption: '{_ Select ... _}', optionsText: 'title', optionsValue: 'id', value: $root.connectionCategory"></select>
                                    <!-- /ko -->
                                </div>
                            </div>
                            <div class="control-group">
                                <label class="control-label">{_ To page attribute _}</label>
                                <div class="controls">
                                    <input type="text" data-bind="value: fieldMapping" placeholder="id, title, summary, ..." />
                                </div>
                            </div>
                            <div class="control-group">
                                <label class="control-label">{_ Read multiple from comma-separated list _}</label>
                                <div class="controls">
                                    <input type="checkbox" data-bind="checked: makelist" />
                                </div>
                            </div>
                        <!-- /ko -->
                        <!-- ko if: $root.predicateMapping() !== undefined && categoryOptions().length === 0 -->
                        <div class="alert alert-error">Select a predicate that allows categories</div>
                        <!-- /ko -->
                    <!-- /ko -->
                </div>
            </form>
        </div>
    </div>
    <div class="modal-footer">
        <a href="#" class="btn" data-bind="click: cancel">Cancel</a>
        <a href="#" class="btn btn-primary" data-bind="click: update">Update settings</a>
    </div>
</div>