# Debug Proxy Workload

The application facilitates development of proxy and helps to script API workload.


## Getting started

`Makefile` is used to orchestrated development. Use the following commands to build a workload scripts and run Erlang shell.

```bash
make compile && make run
```

## Usage

```erlang
%%
%% Initialize the debug environment
application:ensure_all_started(debug).

%%
%% register the developer and api
{ok, #{secret := Secret}} = debug_register:api().

%%
%% issue request to api through proxy
debug_register:request(Secret).
```
