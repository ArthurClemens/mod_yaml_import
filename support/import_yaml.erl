%% @doc Import a YAML file according to settings passed in json data.
%% @author Arthur Clemens <arthur@visiblearea.com>
%% Date: 2013-12-27

%% Copyright 2013 Arthur Clemens
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(import_yaml).

-author("Arthur Clemens <arthur@visiblearea.com>").

-include_lib("zotonic.hrl").

-export([
    import/3
]).

%%-record(importresult, {seen=[], new=[], updated=[], errors=[], ignored=[], deleted=0}).

import(Data, JsonData, Context) ->
    StartDate = erlang:localtime(),
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
    RangeLimitedData = lists:sublist(Data, RangeFrom, (RangeTo-RangeFrom+1)),
        
    ProcessedPages = lists:map(fun(PageData) -> 
        create_resource(PageData, Category, FieldAttrs, TitleField, Context) 
    end, RangeLimitedData),

    ProcessedPagesData = lists:map(fun(PageData) ->
        case PageData of
            [{page, PageId}, {media, AttachedIdList}, {connection, PredicateIdList}] ->
                ConnectionIds = lists:flatten(AttachedIdList) ++ lists:flatten(PredicateIdList),
                [
                    {pagedata, richPageData(PageId, Context)},
                    {connectiondata, [richPageData(ObjectId, Context) || {id, ObjectId} <- ConnectionIds, id =/= error]}
                ];
            [{page, {error, _}}] ->
                % TODO: report errors
                []
            end     
        end, ProcessedPages),
    
    PrunedProcessedPagesData = lists:filter(fun(L) -> L =/= [] end, ProcessedPagesData),
    
    [
        {date_start, binary_date(StartDate)},
        {date_end, binary_date(erlang:localtime())},
        {imported, PrunedProcessedPagesData}
    ].

%% Create Predicate resource
create_resource(PageData, Category, _, TitleField, Context) when Category =:= 117 ->    
    PredicateName = binary_to_list(proplists:get_value(<<"name">>, PageData)),
    FromName = binary_to_list(proplists:get_value(<<"from">>, PageData)),
    FromId = m_rsc:name_lookup(FromName, Context),
    ToName = binary_to_list(proplists:get_value(<<"to">>, PageData)),
    ToId = m_rsc:name_lookup(ToName, Context),
    Title = binary_to_list(proplists:get_value(TitleField, PageData)),

    Props = [
        {title, Title},
        {category, Category},
        {is_published, true},
        {name, PredicateName},
        {predicate_subject, FromId},
        {predicate_object, ToId}
        ],

    try
        {ok, PageId} = m_rsc:insert(Props, Context),
        [{page, PageId}, {media, []}, {connection, []}]
    catch
        {_, Reason} -> 
            [{page, {error, Reason}}]
    end;


%% Create Page or Category resource
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
    
    try
        {ok, PageId} = m_rsc:insert(PageProps, Context),
        AttachedIds = upload_media(CleanMediumProps, PageId, Context),
        ConnectedIds = link_pages(CleanConnectionProps, PageId, Context),
        [{page, PageId}, {media, AttachedIds}, {connection, ConnectedIds}]
    catch
        {_, Reason} -> 
            [{page, {error, Reason}}]
    end.


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
            FieldIdBin = proplists:get_value(<<"field">>, FieldMappingProps, <<"undefined">>),
            FieldId = list_to_atom(unicode:characters_to_list(FieldIdBin, utf8)),
            
%            if 
%                FieldId == undefined -> 
%                    % create Page of category ObjectCategory, with title FieldValue
%                    % TODO: add to processed list
%                    Props = [{title, FieldValue}, {category, ObjectCategory}, {is_published, true}],
%                    m_rsc:insert(Props, Context)
%            end,
            
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
                [Id || Id <- RawList, mod_yaml_import:get_prop(Id, FieldId, Context) =:= Value]
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
            Mode = binary_to_list(proplists:get_value(<<"status">>, FieldMappingProps)),
            {make_valid_field_key(Mode), to_boolean(binary_to_list(FieldValue))};
        "medium" ->
            {undefined, undefined};
        _ ->
            {make_valid_field_key(FieldName), FieldValue}
        end.


%% Changes a key string to a field identifier that:
%% - starts with lowercase
%% - is an atom
%% @spec make_valid_field_key(KeyString) -> SafeKeyAtom
make_valid_field_key(K) ->
    [H|T] = re:replace(K, "\\s+", "", [global,{return,list}]), % strip whitespace
    SafeKey = string:to_lower([H]) ++ T,
    list_to_atom(SafeKey).


richPageData(Id, Context) ->
    [
        {id, to_binary(Id)},
        {url, to_binary(m_rsc:page_url(Id, Context))},
        {edit_url, to_binary(z_dispatcher:url_for(admin_edit_rsc, [{id, Id}], Context))},
        {title, to_binary(mod_yaml_import:get_prop(Id, title, Context))},
        {category, to_binary(m_rsc:p(m_rsc:p(Id, category_id, Context), name, Context))}
    ].


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
            upload_medium(PageId, z_convert:to_list(Url), Context)
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


binary_date(DateTime) ->
    z_convert:to_binary(io_lib:format("~s", [iso_8601_fmt(DateTime)])).

    
iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).
    
    
json_decode(V) when is_binary(V) -> unicode:characters_to_list(V, utf8);
json_decode(V) when is_list(V) -> V;
json_decode(V) when is_atom(V) -> V;
json_decode(V) -> V.

to_boolean(V) when is_list(V), V =:= "true" -> true;
to_boolean(V) when is_list(V), V =:= "false" -> false;
to_boolean(V) when is_list(V), V =:= "1" -> true;
to_boolean(V) when is_list(V), V =:= "0" -> false;
to_boolean(V) when is_list(V), V =:= [] -> false;
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
