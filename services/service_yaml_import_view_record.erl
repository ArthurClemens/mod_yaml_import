-module(service_yaml_import_view_record).
-author("Arthur Clemens").

-svc_title("View record value data").
-svc_needauth(true).

-export([process_post/2]).

-include_lib("zotonic.hrl").

process_post(_ReqData, Context) ->
    PageNumStr = z_context:get_q("pageNum", Context),
    {PageNum, _} = string:to_integer(PageNumStr),
    Record = mod_yaml_import:record_values(PageNum, Context),
    Json = z_convert:to_json(Record),
    Json.