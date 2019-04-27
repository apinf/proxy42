%% @doc
%%   debug api registration workflow
-module(p42_debug_register).
-compile({parse_transform, category}).

-export([
   api/0,
   request/1
]).

-define(HOST, "http://localhost:4001").

%%
%% @doc
%%   registers example api
api() ->
   m_http:once(
      [m_state ||
         Developer <- register_developer(),
         Secret <- issue_secret_key(Developer),
         Api <- register_api(),
         _ <- authorize_developer(Api, Developer),
         cats:unit(#{
            developer => Developer
         ,  secret    => Secret
         ,  api       => Api
         })
      ]
   ).

register_developer() ->
   [m_http ||
      _ > {'POST', ?HOST ++ "/developers"},
      _ > "Accept: application/json",
      _ > "Content-Type: application/json",
      _ > #{<<"email">> => <<"test@apinf.io">>},

      _ < 201,
      _ < lens:at(<<"id">>)
   ].

issue_secret_key(Developer) ->
   [m_http ||
      _ > {'POST', ?HOST ++ "/plugins/auth_key/issue_key"},
      _ > "Accept: application/json",
      _ > "Content-Type: application/json",
      _ > #{<<"developer_id">> => Developer},

      _ < 201,
      _ < lens:at(<<"key">>)
   ].

register_api() ->
   [m_http ||
      _ > {'POST', ?HOST ++ "/apis"},
      _ > "Accept: application/json",
      _ > "Content-Type: application/json",
      _ > #{
         <<"hostname">> => <<"httpbin.org">>,
         <<"servers">> => [<<"http://httpbin.org">>],
         <<"frontend_prefix">> => <<"/awesome-api/">>,
         <<"backend_prefix">> => <<"/">>,
         <<"strategy">> => <<"random">>,
         <<"rate_limit">> => 43,
         <<"additional_headers">> => <<"">>,
         <<"auth_config">> => #{<<"strategy">> => <<"auth_key">>}
      },

      _ < 201,
      _ < lens:at(<<"id">>)
   ].

authorize_developer(Api, Developer) ->
   [m_http ||
      _ > {'POST', ?HOST ++ "/authorizations"},
      _ > "Accept: application/json",
      _ > "Content-Type: application/json",
      _ > #{<<"developer_id">> => Developer, <<"api_id">> => Api},

      _ < 201
   ].

%%
%%
request(Key) ->
   m_http:once(
      [m_http ||
         _ > "GET http://localhost:8080/awesome-api/get",
         _ > "Host: httpbin.org",
         _ > "Authorization: Bearer " ++ erlang:binary_to_list(Key),

         _ < 200,
         _ < '*'
      ]
   ).

