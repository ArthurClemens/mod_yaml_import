-module(service_yaml_import_validate_setting).
-author("Arthur Clemens").

-svc_title("Validates import settings").
-svc_needauth(true).

-export([process_post/2]).

-include_lib("zotonic.hrl").

process_post(_ReqData, Context) ->
    % TODO: handle invalid json
    Json = z_context:get_q("json", Context),
    Struct = mochijson2:decode(Json),    
    {struct, JsonData} = Struct,
    FieldBin = proplists:get_value(<<"field">>, JsonData),
    FieldName = binary_to_list(FieldBin),
    ValueBin = proplists:get_value(<<"value">>, JsonData),
    FieldValue = binary_to_list(ValueBin),
    
    Result = mod_yaml_import:validate(FieldName, FieldValue, Context),
    
    case Result of
        {ok, Results} -> 
            {struct, [{result, z_convert:to_json(Results)}]};
        {_, Reason} ->
            % do not throw a response error
            {struct, [{result, z_convert:to_json(Reason)}]}
        end.