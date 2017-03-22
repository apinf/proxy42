Design of Proxy42
=================

This document will eventually evolve into a detailed description of Proxy42's
architecture and design. For some time it may remain a jumbled account of what
is being planned.

The primary aim of Proxy42 is to act as a HTTP proxy.

Initial thoughts:

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

Each incoming request is handled in a process of its own. This provides
excellent isolation at little to no performance penalty thanks to Erlang VM.

A brief outline of steps involved in proxying a request are as follows

#. Acceptor pool accepts incoming requests, performs ssl termination if desired.
#. Spawns a new process immediately and hands the socket over.
#. The process is a gen fsm with the following phases. (gen_fsm vs gen_server?)

   - Parse headers and url
   - Identify config: target upstream server, auth, rate limits
   - Perform auth: Pluggable adapters? JWT based? Stored tokens?
   - Check rate limits
   - Forward to admin app or proxy app as appropriate.
   - Initiate proxying, wait for response
   - Add ``X-RateLimit-*`` and other headers, stream response
   - Clean up and close the connection

   Timestamps are recorded at each step in the process state, and sent to
   analytics subsystem

Analytics subsystem uses pluggable adapters. The adapters are expected to
handle the following data:

#. Counters for requests received, bytes transferred, etc
#. Consolidated timing information.

We will very likely have a default adapter backed by ets/mnesia with limited
functionality, and a few adapters to stream the events or their aggregates to
various tools over TCP/UDP.

We need to store time taken per phase for each request to be able to chart the
trends.  This data may be compacted into summaries (mean, median, stddev, etc)
once in a while to save space. Details yet to be worked out.
