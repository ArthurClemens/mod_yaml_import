%% @author Arthur Clemens
%% @copyright 2013 Arthur Clemens
%% Generated on 2013-08-16
%% @doc YAML data import

-module(mod_yaml_import).
-author("Arthur Clemens").

-mod_title("YAML import").
-mod_description("Import YAML data into Zotonic.").
-mod_prio(500).
-mod_depends([base, admin]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
         init/1,
         observe_admin_menu/3,
         event/2,
         record_values/2,
         category_list/2,
         meta_category_list/1,
         predicate_list/1,
         validate/3,
         get_prop/3
]).


-define(KEY_STORAGE_DATA, "yaml_import_data").


init(_Context) ->
	application:start(yamerl),
	ok.


observe_admin_menu(admin_menu, Acc, _Context) ->
    [
     #menu_item{id=yaml_import,
                parent=admin_modules,
                label="Import YAML data",
                url={admin_yaml_import},
                visiblecheck={acl, use, mod_yaml_import}}
     |Acc].


%% Submitted form, either with JSON data or without.
%% If no JSON is passed, we use the file to populate the settings screen.
event(#submit{message={import, _}}, Context) ->
    File = z_context:get_q("filename", Context),
    Json = z_context:get_q("json", Context),
    case Json of
        undefined ->
            handleFileUpload(File, Context);
        [] ->
            handleFileUpload(File, Context);
        _ ->
            handleImport(File, Json, Context)
        end.


%% Reads uploaded YAML file and stores the data in persistent
%% Invokes action 'init_record_structure' to call JavaScript with the data structure (not the contents).
handleFileUpload(File, Context) ->
    Data = parseFile(File),
    case Data of
        undefined -> 
            Html = z_template:render("_admin_yaml_import_error.tpl", [{message, io_lib:format("Could not parse file ~p", [File#upload.filename])}], Context),
            z_render:update_selector(".admin-yaml-import-upload-error", Html, Context);
        _ ->
            % Store data for later when user decides to import
            z_context:set_persistent(?KEY_STORAGE_DATA, Data, Context), 

            PageNum = 1,
            Page = lists:nth(PageNum, Data),
            PageCount = length(Data),
            Keys = lists:map(fun({K,_}) -> K end, Page),    
        
            % Remove possible leftover error message
            Context1 = z_render:update_selector(".admin-yaml-import-upload-error", "", Context),
        
            % Show form page
            Html = z_template:render("_step_settings_form.tpl", [{pageCount, PageCount}], Context1),
            Context2 = z_render:update_selector(".admin-yaml-import-form", Html, Context1), 

            % Pass record structure to javascript    
            Record = lists:map(fun(K) -> 
                [
                    {name, K},
                    {included, true}
                ]
                end, Keys),
            Json = mochijson2:encode(Record),
            z_render:wire({init_record_structure, [{json, Json}]}, Context2)
        end.


handleImport(File, Json, Context) ->
    Data = parseFile(File),
    Context1 = case Data of
        undefined -> 
            z_render:growl_error("No data found", Context);
        _ ->
            {struct, JsonData} = mochijson2:decode(Json),
            Result = handle_spawn(Data, JsonData, Context),
            ResultJson = mochijson2:encode(Result),
            z_render:wire({record_imported, [{json, ResultJson}]}, Context)
        end,
    Context1.


parseFile(File) ->
    {ok, FileData} = file:read_file(File#upload.tmpfile),
    FileString = unicode:characters_to_list(FileData, utf8),
    case FileString of
        [] -> undefined;
        _ -> 
            % Parse data
            FileString1 = FileString ++ "\n",
            try yamerl_constr:string(FileString1, [str_node_as_binary]) of
                Docs -> Docs,
                hd(Docs)
            catch
                _ -> undefined
            end
        end.


handle_spawn(Data, JsonData, Context) ->
    Parent = self(),
    Child = spawn_link(fun() -> Parent ! {self(), apply(import_yaml, import, [Data, JsonData, Context])} end),
    receive
        {Child, X} -> X
        end.



get_prop(Id, Prop, Context) ->
    RawValue = case m_rsc:p(Id, Prop, Context) of
            {trans, _} = T -> z_trans:lookup(T, Context);
            Value -> Value
        end,
    to_binary(RawValue).


%% Retrieves 1 page/record of data values.
%% @spec record_values(PageNum, Context) -> Props
record_values(PageNum, Context) ->
    % assumes data exists in persistent
    %% TODO: error check unpacking data
    Data = z_context:get_persistent(?KEY_STORAGE_DATA, Context),
        
    PageNum1 = min_num(PageNum, 1),
    PageNum2 = max_num(PageNum1, length(Data)),
    
    Page = lists:nth(PageNum2, Data),

    Props = [
        {pageNum, PageNum2},
        {hasNext, (PageNum2 =/= length(Data))},
        {hasPrevious, (PageNum2 =/= 1)},
        {values, Page}
    ],
    Props.


category_list(PredicateId, Context) ->
    case PredicateId of
        undefined ->
            FlatList = m_category:all_flat(Context),
            lists:map(fun({Id, _, _, _}) ->
                [Id, get_prop(Id, title, Context)]
            end, FlatList);
        _ ->
            CategoryList = m_predicate:objects(PredicateId, Context),
            lists:map(fun(Id) ->
                [Id, get_prop(Id, title, Context)]
            end, CategoryList)
        end.


meta_category_list(Context) ->
    {_, CategoryId} = m_rsc:name_to_id("category", Context),
    {_, PredicateId} = m_rsc:name_to_id("predicate", Context),
    MetaCategoryList = [CategoryId, PredicateId],
    lists:map(fun(Id) ->
        [Id, get_prop(Id, title, Context)]
    end, MetaCategoryList).


predicate_list(Context) ->
    PredicateList = m_predicate:all(Context),
    lists:map(fun({_, Props}) ->
        Id = proplists:get_value(id, Props),
        [Id, get_prop(Id, title, Context)]
    end, PredicateList).


validate(FieldName, FieldValue, Context) ->
    case FieldName of
        "fieldName" -> validate_fieldName(FieldValue, Context);
        "rangeTo" -> validate_range(FieldValue, Context);
        "rangeFrom" -> validate_range(FieldValue, Context);
        _ -> {error, "Unknown field"}
    end.

validate_fieldName(FieldValue, _) 
    when FieldValue =:= ""
    -> {error, "Enter a name"};

validate_fieldName(FieldValue, _) 
    when   FieldValue =:= "id"
    orelse FieldValue =:= "title"
    orelse FieldValue =:= "tag"
    orelse FieldValue =:= "category"
    orelse FieldValue =:= "translation"
    orelse FieldValue =:= "modified"
    -> {error, "This name is reserved"};
    
validate_fieldName(FieldValue, _) ->
    [H|_] = FieldValue,
    case (H == string:to_lower(H)) of
        false -> {error, "Must start with lowercase"};
        true -> 
            RegExp = "^[a-z][a-zA-Z_]*$",
            case re:run(FieldValue, RegExp) of
                {match, _} -> {ok, 1};
                nomatch -> {error, "Not a valid name"}
            end
    end.
    
validate_range(FieldValue, Context) ->
    {Value, Rest} = string:to_integer(FieldValue),
    case Value of
        error -> 
            {error, "Enter a number"};
        _ ->
            case (Rest == []) of 
                false -> {error, list_to_binary("Enter a rounded number")};
                true -> 
                    Data = z_context:get_persistent(?KEY_STORAGE_DATA, Context),
                    Max = length(Data),
                    case (Value >= 1 andalso Value =< Max) of
                        true -> {ok, 1};
                        false -> {error, list_to_binary(io_lib:format("Enter a number between 1 and ~p", [Max]))}
                    end
            end
    end.
    
    
%% Returns the lowest number
min_num(N, Min) when N < Min ->
    Min;
min_num(N, _) ->
    N.

%% Returns the highest number
max_num(N, Max) when N > Max ->
    Max;
max_num(N, _) ->
    N.

to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> list_to_binary(integer_to_list(V));
to_binary(V) -> V.

