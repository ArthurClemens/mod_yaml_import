-module(action_yaml_import_yi_init_structure).
-include("zotonic.hrl").

-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) -> 
    Json = z_convert:to_list(proplists:get_value(json, Args, "")),
    Script = [<<"mod_yaml_import.initRecord(\"">>,z_utils:js_escape(Json),<<"\");">>],
	{Script, Context}.
