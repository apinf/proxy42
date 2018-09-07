-module(p42_req).
-compile([export_all]).

-type req() :: cowboyku_req:req().
-type body() :: binary().
-type path() :: iodata().
-type method() :: binary().
-type header() :: binary().
-type hdrval() :: binary() | undefined.
-type headers() :: cowboyku:http_headers().
-type qs() :: binary().
-type param() :: binary().
-type qs_val() :: binary() | true | undefined.
-type req_id() :: erequest_id:request_id().
-type response_code() :: non_neg_integer().
-type peer() :: undefined | {inet:ip_address(), inet:port_number()}.
-type logging() :: [{erlang:timestamp(), atom(), atom()}].
-export_type([req/0, path/0, method/0, header/0, hdrval/0,
              headers/0, qs/0, qs_val/0, req_id/0, response_code/0,
              peer/0, logging/0]).

%% TODO: See if ignoring _Req makes sense for all functions.
%% qs_val/3, qs_vals/2 are memoized calls in cowboyku_req.
%% path/1, method/1, header/1, headers/1, qs/1, qs_val/2,
%% meta/2, meta/3, peer/1 do not modify Req, so its okay to ignore
%% returned value.

-spec get_path(req()) -> path().
get_path(Req) ->
  {Path, _Req} = cowboyku_req:path(Req),
  Path.

-spec get_method(req()) -> method().
get_method(Req) ->
  {Method, _Req} = cowboyku_req:method(Req),
  Method.

-spec get_header(header(), req()) -> hdrval().
get_header(Hdr, Req) ->
  {HdrVal, _Req} = cowboyku_req:header(Hdr, Req),
  HdrVal.

-spec get_headers(req()) -> headers().
get_headers(Req) ->
  {Headers, _Req} = cowboyku_req:headers(Req),
  Headers.

-spec get_qs(req()) -> qs().
get_qs(Req) ->
  {Qs, _Req} = cowboyku_req:qs(Req),
  Qs.

-spec get_qs_val(param(), req()) -> qs_val().
get_qs_val(Param, Req) ->
  {QSVal, _Req} = cowboyku_req:qs_val(Param, Req),
  QSVal.

-spec get_logging(req()) -> logging().
get_logging(Req) ->
  {Log, _Req} = cowboyku_req:meta(logging, Req),
  {log, InitTime, StampsQ} = Log,
  Stamps = queue:to_list(StampsQ),
  [{InitTime, init, no_details} | Stamps].

-spec get_req_id(req()) -> req_id().
get_req_id(Req) ->
  {ReqId, _Req} = cowboyku_req:meta(request_id, Req),
  ReqId.

-spec get_response_code(req()) -> response_code().
get_response_code(Req) ->
  {RespCode, _Req} = cowboyku_req:meta(response_code, Req),
  RespCode.

-spec incoming_peer(req()) -> peer().
incoming_peer(Req) ->
  {IncomingPeer, _Req} = cowboyku_req:peer(Req),
  IncomingPeer.
