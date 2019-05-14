---
id: request-flow
title: Request Flow
---

When Proxy42 recieves a request, it passes through various "phases". It is
helpful to understand these phases when developing plugins for Proxy42 or when
working on the codebase.


 1. Incoming requests are accepted by a pool of ranch acceptors.
 1. Cowboyku takes control of the request, parses it, and transfers control to
    vegur.
 1. Vegur runs `vegur_midjan_middleware` to process the request.
    `vegur_request_log:{new/1,done/4}` are run at the beginning and end of
    request respectively.
 1. Midjan is an ececutor. It can execute a chain of modules, going back and
    forth if necessary. Midjan executes the following middleware chain, in the
    listed order. Some of these middlewares expect a callback function on an
    "Interface Module", which can maintain an internal "Handler State" to
    configure their behaviour.
    1. `vegur_validate_headers` - Performs basic header validation
    1. `vegur_lookup_domain_middleware` - Looks up the API for request
    1. `proxy42_authenticate_middleware` - Handles request authentication and
       authorization. Ratelimits are also handled here for now.
    1. `vegur_continue_middleware` - Intercepts and responds to 100 Continue
    1. `vegur_upgrade_middleware` - Handles connection upgrades - ex: to
       websockets
    1. `vegur_lookup_service_middleware` - Picks a server to send the request
       to. Load balancing/shaping is implemented by its callback.
    1. `vegur_proxy42_middleware` - Establishes connection to API server and
       proxies request and response.

    Some of the names might be non intuitive since we adapted vegur's default
    stack with minimal changes - Some modules will be renamed or rewritten in
    near future. In particular, some existing logic in router will be moved
    into middlewares, lookup_domain will be renamed to lookup_api, and applying
    hierarchial settings will be the responsibility of middlewares rather than
    router.

The project has a single interface module, `proxy42_router`. This should be the
entrypoint for contributors when getting started on the project.  The following
functions/callbacks are exported by the interface module, and are listed in the
order in which they will be executed.

1. `init/2` - Called by `vegur_req_log:new/1` to initialize router state. This
   is called Request Context (ReqCtx) throughout the router, and HandlerState
   in the middleware.
1. `lookup_domain_name/3` - Called by `vegur_lookup_domain_middleware`. Domain
   level and API level settings are applied here.
1. `auth/2` - Called by `proxy42_authenticate_middleware` to perform
   authentication and authorization. The actual authentication is delegated to
   authentication plugins. Developer and Application User level settings are
   applied here.
1. `rate_limit/3` - Called by `proxy42_authenticate_middleware` based on result
   of `auth/2`. Ratelimit plugin in effect will be called to record the request
   and check quota.
1. `checkout_service/3` - Called by `vegur_lookup_service_middleware` to pick a
   server from available backend API servers for requested API.
1. `checkin_service/6` - Called by `vegur_lookup_service_middleware` just
   before releasing the backend server into the pool - after request is
   successfully completed or before requesting a new checkout if the chosen
   server is unresponsive.
1. `service_backend/3` - Called by `vegur_lookup_service_middleware` to resolve
   the chosen server into an ip address and port.
1. `backend_request_params/3` - Called by `vegur_proxy42_middleware` to
   customize request parameters. Request path is rewritten if necessary.
1. `transform_response_headers/2` - Called by `vegur_proxy42_middleware` to
   customize response headers. Host header is rewritten in particular.
1. `additional_headers/4` - Called by `vegur_proxy42_middleware` to customize
   headers sent in either direction.
1. `error_page/4` - Translates internal errors from middlewares to HTTP
   statuses. Called by middlewares via `vegur_utils:handle_error/2`.
1. `terminate/3` - Called at the end of request lifecycle via `vegur_request_log:done/4`.
1. `feature/2` - The behaviour of `vegur_proxy42_middleware` and
   `vegur_continue_middleware` can be customized by defining feature flags. We
   do not use this currently.
