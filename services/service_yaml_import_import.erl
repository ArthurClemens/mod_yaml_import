-module(service_yaml_import_import).
-author("Arthur Clemens").

-svc_title("Pass save settings for importing").
-svc_needauth(true).

-export([process_post/2]).

-include_lib("zotonic.hrl").

process_post(_Req, Context) ->
    % TODO: handle invalid json
    Json = z_context:get_q("json", Context),
    {struct, JsonData} = mochijson2:decode(Json),
    
    Result = mod_yaml_import:import(JsonData, Context),
    case Result of
        {ok, Results} -> 
            {struct, [{results, z_convert:to_json(Results)}]};
        _ ->
            {error, not_exists, "uploaded data"}
        end.
