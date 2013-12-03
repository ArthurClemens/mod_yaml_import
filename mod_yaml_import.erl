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
         import/2,
         category_list/2,
         meta_category_list/1,
         predicate_list/1,
         validate/3
]).

-define(KEY_STORAGE_DATA, "yaml_import_data").

init(_Context) ->
	% Setup module specific stuff here
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


%% Reads uploaded YAML file and stores the data in persistent
%% Invokes action 'yi_init_structure' to call JavaScript with the data structure (not the contents).
event(#submit{message={upload, _}}, Context) ->
    Upload = z_context:get_q("filename", Context),
    
    % TODO: error check read file
    {ok, File} = file:read_file(Upload#upload.tmpfile),
    FileString = unicode:characters_to_list(File, utf8),
    
    % Parse data
    try yamerl_constr:string(FileString, [str_node_as_binary]) of
        Docs -> Docs,
        Data = hd(Docs),
        % Store data for later when user decides to import
        z_context:set_persistent(?KEY_STORAGE_DATA, Data, Context), 
                
        PageNum = 1,
        Page = lists:nth(PageNum, Data),
        PageCount = length(Data),
        Keys = lists:map(fun({K,_}) -> K end, Page),    
        TemplateProps = [{pageCount, PageCount}],
        
        % Remove possible leftover error message
        Context1 = z_render:update_selector(".admin-yaml-import-upload-error", "", Context),
        
        % Show form page
        Html = z_template:render("_admin_yaml_import_settings.tpl", TemplateProps, Context1),
        Context2 = z_render:update_selector(".admin-yaml-import-form", Html, Context1), 

        % Pass record structure to javascript    
        Record = lists:map(fun(K) -> 
            [
                {name, K},
                {included, true}
            ]
            end, Keys),
        Json = mochijson2:encode(Record),
        z_render:wire({yi_init_structure, [{json, Json}]}, Context2)
    catch
        _ -> 
            TemplateProps = [
                {message, io_lib:format("Could not parse file ~p", [Upload#upload.filename])}
            ],
            Html = z_template:render("_admin_yaml_import_error.tpl", TemplateProps, Context),
            z_render:update_selector(".admin-yaml-import-upload-error", Html, Context)
    end.
    

%% Imports the data that was already uploaded and stored in persistent
%% according to settings passed in json data.
%% Returns import results.
%% @spec import(JsonData, Context) -> [Results]
import(JsonData, Context) ->

    Data = z_context:get_persistent(?KEY_STORAGE_DATA, Context),
        
    case Data of 
        undefined -> undefined;
        [] -> undefined;
        _ ->    
            TitleField = proplists:get_value(<<"titleField">>, JsonData), % keep as binary as lookup key in page data
            Category = proplists:get_value(<<"category">>, JsonData),
            RangeFrom = list_to_integer(binary_to_list(proplists:get_value(<<"rangeFrom">>, JsonData))),
            RangeTo = list_to_integer(binary_to_list(proplists:get_value(<<"rangeTo">>, JsonData))),
    
            FieldsStructs = proplists:get_value(<<"fields">>, JsonData),
            FieldAttrs = lists:map(fun({struct, FieldData}) -> 
                lists:map(fun({K, V}) ->
                    {K, json_decode(V)}
                    end, FieldData)
                end, FieldsStructs),
        
            RangeData = lists:sublist(Data, RangeFrom, (RangeTo-RangeFrom+1)),
    
            ProcessedPages = lists:map(fun(PageData) -> 
                create_resource(PageData, Category, FieldAttrs, TitleField, Context) 
                end, RangeData),
        
            ProcessedPagesData = lists:map(fun([{page, PageId}, {media, AttachedIdList}, {connection, PredicateIdList}]) ->
                ConnectionIds = lists:flatten(AttachedIdList) ++ lists:flatten(PredicateIdList),
                [
                    {pagedata, richPageData(PageId, Context)},
                    {connectiondata, [richPageData(ObjectId, Context) || {id, ObjectId} <- ConnectionIds, id =/= error]}
                ]
                end, ProcessedPages),
        
            {ok, ProcessedPagesData}
            end.


richPageData(Id, Context) ->
    [
        {id, to_binary(Id)},
        {url, to_binary(m_rsc:page_url(Id, Context))},
        {edit_url, to_binary(z_dispatcher:url_for(admin_edit_rsc, [{id, Id}], Context))},
        {title, to_binary(get_prop(Id, title, Context))},
        {category, to_binary(m_rsc:p(m_rsc:p(Id, category_id, Context), name, Context))}
    ].

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


create_resource(PageData, Category, FieldAttrs, TitleField, Context) ->
    
    Title = proplists:get_value(TitleField, PageData),
    
    ValueProps = [make_page_prop(page, PageData, Attrs, Context) || Attrs <- FieldAttrs],
    MediumProps = [make_page_prop(medium, PageData, Attrs, Context) || Attrs <- FieldAttrs],
    ConnectionProps = [make_page_prop(connection, PageData, Attrs, Context) || Attrs <- FieldAttrs],
        
    CleanValueProps = lists:filter(fun({K,_}) -> K =/= undefined end, ValueProps),
    CleanMediumProps = lists:filter(fun({K,_}) -> K =/= undefined end, MediumProps),
    CleanConnectionProps = lists:filter(fun({K,_,_}) -> K =/= undefined end, ConnectionProps),
    
    Props = [{title, Title},
             {category, Category},
             get_published_state(CleanValueProps)],
    
    PageProps = Props ++ CleanValueProps,
    
    PageIds = case m_rsc:insert(PageProps, Context) of
        {ok, PageId} -> 
            AttachedIds = upload_media(CleanMediumProps, PageId, Context),
            ConnectedIds = link_pages(CleanConnectionProps, PageId, Context),
            [{page, PageId}, {media, AttachedIds}, {connection, ConnectedIds}];
        {error, Reason} ->
            [{page, {error, Reason}}]
        end,
    PageIds.


%% Changes a key string to a field identifier that:
%% - starts with lowercase
%% - is an atom
%% @spec make_valid_field_key(KeyString) -> SafeKeyAtom
make_valid_field_key(K) ->
    [H|T] = re:replace(K, "\\s+", "", [global,{return,list}]), % strip whitespace
    SafeKey = string:to_lower([H]) ++ T,
    list_to_atom(SafeKey).


make_page_prop(Type, PageData, Attrs, Context) ->
    FieldId = proplists:get_value(<<"id">>, Attrs),
    FieldName = proplists:get_value(<<"name">>, Attrs),
    FieldType = proplists:get_value(<<"type">>, Attrs),
    FieldValue = proplists:get_value(list_to_binary(FieldId), PageData),
    FieldMapping = proplists:get_value(<<"mapping">>, Attrs, {struct, []}),
    {struct, FieldMappingProps} = FieldMapping,
    MakeList = proplists:get_value(<<"makelist">>, FieldMappingProps),
    make_page_prop(Type, FieldName, FieldType, FieldValue, FieldMappingProps, MakeList, Context).

make_page_prop(Type, _, FieldType, FieldValue, FieldMappingProps, MakeList, Context) when Type =:= connection ->    
    case FieldType of
        "connection" -> 
            ObjectCategory = proplists:get_value(<<"category">>, FieldMappingProps),
            PredicateId = proplists:get_value(<<"predicate">>, FieldMappingProps),
            FieldIdBin = proplists:get_value(<<"field">>, FieldMappingProps),
            FieldId = list_to_atom(unicode:characters_to_list(FieldIdBin, utf8)),
                        
            % Create list of object ids from category ObjectCategory that have the value FieldValue
            
            ObjectValues = case MakeList of
                true -> make_list(FieldValue);
                _ -> [FieldValue]
                end,
            
            % get all pages within the object category
            #search_result{result=RawList} = z_search:search({all, [{cat,ObjectCategory}]}, Context),
            
            ObjectIds = lists:map(fun(V) ->                                
                Value = case MakeList of
                    true -> 
                        % convert back to binary
                        % should be optimized though
                        list_to_binary(V);
                    _ -> V
                    end,

                % filter out all pages where the value of the field does not correspond to the value of the current object
                [Id || Id <- RawList, get_prop(Id, FieldId, Context) =:= Value]
                end, ObjectValues),
            {connection, PredicateId, lists:flatten(ObjectIds)};
        _ -> {undefined, undefined, undefined}
        end;

make_page_prop(Type, _, FieldType, FieldValue, _, MakeList, _) when Type =:= medium ->
    case FieldType of
        "medium" ->
            case MakeList of
                true -> {medium_url, make_list(FieldValue)};
                _ -> {medium_url, [FieldValue]}
                end;
        _ -> {undefined, undefined}
        end;

make_page_prop(_, FieldName, FieldType, FieldValue, FieldMappingProps, _, _) ->
    case FieldType of
        "status" ->
            {struct, [{<<"status">>, ModeBin}]} = FieldMappingProps,            
            Mode = binary_to_list(ModeBin),
            {make_valid_field_key(Mode), to_boolean(FieldValue)};
        "medium" ->
            {undefined, undefined};
        _ ->
            {make_valid_field_key(FieldName), FieldValue}
        end.

    
make_list(V) when is_binary(V) ->
    make_list(unicode:characters_to_list(V, utf8));
make_list(V) when is_list(V) ->
    case is_string(V) of
        false ->
            lists:map(fun(Item) ->
                hd(make_list(Item))
            end, V);
        true ->
            re:split(V, "\s*,\s*", [{return,list}])
        end.


get_published_state(Props) ->
    case proplists:get_value(is_published, Props) of
        true -> {undefined, undefined};
        false -> {undefined, undefined};
        _ -> {is_published, true}
        end.


%% Fetches (optionally multiple) media items from a list of urls and attaches each downloaded medium to the specified page.
%% Props list only contains tuples of format {medium_url, [Urls]}.
%% Returns a list of m_edge:insert results.
%% @spec upload_media(Props, PageId, Context) -> [[{ok, Id} | {error, Reason}]]
upload_media(Props, PageId, Context) ->
    lists:map(fun({_, Urls}) -> 
        lists:map(fun(Url) ->
            upload_medium(PageId, Url, Context)
        end, Urls)
    end, Props).

upload_medium(PageId, UrlString, Context) ->
    case is_string(UrlString) of
        true ->
            [connect_medium_to_page(UrlString, PageId, Context)];
        false -> 
            lists:map(fun(Url) -> 
                connect_medium_to_page(Url, PageId, Context)
            end, UrlString)
        end.

%% Fetches one media item and returns its id.
%% @spec connect_medium_to_page(Url, PageId, Context) -> {ok, Id} | {error, Reason}
connect_medium_to_page(Url, PageId, Context) ->
    case m_media:insert_url(Url, [], [{escape_texts, false}], Context) of
        {ok, MediaId} -> 
            m_edge:insert(PageId, depiction, MediaId, Context),
            {id, MediaId};
        {error, Reason} ->
            {error, Reason}
        end.


link_pages(Props, SubjectId, Context) ->
    lists:map(fun(P) ->
        {_, PredicateId, ObjectIds} = P,
        lists:map(fun(ObjectId) ->
            % We just created a new page, so no need to check existence
            case m_edge:insert(SubjectId, PredicateId, ObjectId, Context) of
                {ok, _} -> {id, ObjectId}; % for reporting we are not interested in the edge but in the linked object
                {error, Reason} -> {error, Reason}
                end
            end, ObjectIds)
        end, Props).


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

json_decode(V) when is_binary(V) -> unicode:characters_to_list(V, utf8);
json_decode(V) when is_list(V) -> V;
json_decode(V) when is_atom(V) -> V;
json_decode(V) -> V.

to_boolean(V) when is_list(V), V =:= "true" -> true;
to_boolean(V) when is_list(V), V =:= "false" -> false;
to_boolean(V) when is_list(V) -> list_to_integer(V);
to_boolean(V) when is_boolean(V) -> V;
to_boolean(V) when is_integer(V), V =:= 1 -> true;
to_boolean(V) when is_integer(V) -> false;
to_boolean(V) when is_tuple(V), V == true -> true;
to_boolean(V) when is_tuple(V), V == false -> false;
to_boolean(_) -> true.

to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> list_to_binary(integer_to_list(V));
to_binary(V) -> V.

is_string([]) -> true;
is_string([X|T]) -> is_integer(X) andalso X>=0 andalso is_string(T);
is_string(_) -> false.
