Design of Proxy42
=================

This document will eventually evolve into a detailed description of Proxy42's
architecture and design. For some time it may remain a jumbled account of what
is being planned.

The primary aim of Proxy42 is to act as a HTTP proxy.

Initial thoughts and design goals:

- Pure erlang/otp application, that can be built into a Xen Unikernel via
  Erlang on Xen, and an executable via HiPE.

- Distributed by default, and should scale well (linearly) both horizontally
  and vertically.

- Any components using erlang's distribution should be resilient to netsplits
  or unavailability of other nodes, and should gracefully recover from
  conflicts during rejoins.

- Malicious or malformed requests are expected, and should not affect normal
  behaviour.

- Changes to configuration like endpoints, auth/acl/ratelimit rules etc must be
  supported via HTTP API at runtime with immediate effect.

- Should support SSL termination, within the proxy, with an option to use an
  external SSL terminator in front of the proxy.

- Should support websocket proxying, with an option for intercepting messages
  within Proxy42


During the early stages of development, some of the goals evolved, and others had to be abandoned/postponed indefinitely.

HTTP parsing is nontrivial, even if it looks simple at first glance. It is
already implemented in libraries like cowlib, redoing the work is pointless.
We can build on top of existing erlang webservers. After a few exploratory
investigations, we settled on vegur (backed by cowboyku - a fork of cowboy 0.9)
to handle low level http interactions and to focus on developing routing,
control and metrics.

Current status
==============
We currently support only plain HTTP 1.1. Mnesia is used for internal storage.
Clustering is not officially supported yet, though it is kept in mind when
designing the components. A minimal Admin API exists, with plans of overhauling
it entirely.
