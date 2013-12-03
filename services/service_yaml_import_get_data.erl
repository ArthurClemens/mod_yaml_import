-module(service_yaml_import_get_data).
-author("Arthur Clemens").

-svc_title("Gets data from server").
-svc_needauth(true).

-export([process_post/2]).

-include_lib("zotonic.hrl").

%% Returns category list or predicate list
process_post(_ReqData, Context) ->
    % TODO: handle invalid json
    Json = z_context:get_q("json", Context),
    {struct, JsonData} = mochijson2:decode(Json),
    DataType = binary_to_list(proplists:get_value(<<"type">>, JsonData)),

    Result = case DataType of
        "categories" -> 
            DataFilterBin = proplists:get_value(<<"filter">>, JsonData),
            {ok, mod_yaml_import:category_list(DataFilterBin, Context)};
        "meta_categories" ->
            {ok, mod_yaml_import:meta_category_list(Context)};
        "predicates" ->
            {ok, mod_yaml_import:predicate_list(Context)};
        _ ->
            {error, "Data type not found"}
        end,
    
    case Result of
        {ok, Results} -> 
            {struct, [{result, z_convert:to_json(Results)}]};
        {_, Reason} ->
            {error, not_exists, Reason}
        end.