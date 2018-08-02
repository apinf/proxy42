-module(p42_req).
-compile([export_all]).

get_path(Req) ->
  cowboyku_req:path(Req).

get_method(Req) ->
  cowboyku_req:method(Req).

get_header(Hdr, Req) ->
  {Val, _Req} = cowboyku_req:header(Hdr, Req),
  Val.

get_headers(Req) ->
  cowboyku_req:headers(Req).

get_qs(Req) ->
  cowboyku_req:qs(Req).

get_qs_val(Param, Req) ->
  cowboyku_req:qs_val(Param, Req).

get_logging(Req) ->
  {Log, _Req} = cowboyku_req:meta(logging, Req),
  Log.

get_req_id(Req) ->
  {ReqId, _Req} = cowboyku_req:meta(request_id, Req),
  ReqId.

get_response_code(Req) ->
  {RespCode, _Req} = cowboyku_req:meta(response_code, Req),
  RespCode.

incoming_peer(Req) ->
  {IncomingPeer, _Req} = cowboyku_req:peer(Req),
  IncomingPeer.
