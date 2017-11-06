# Proxy42

Note: Proxy42 is a work in progress, and is currently not recommended for
production use.

Proxy42 is an open source high-performance API Gateway. Working in conjunction
with APInf platform, it is designed to make APIs and microservices easy to
manage by providing load balancing, authentication, rate limiting, content
transformation and aggregation, logging and monitoring.

Proxy42 runs on the erlang VM, which provides a solid base for building
distributed, low-latency, soft-realtime systems, and is built on vegur, the
router powering Heroku.

Proxy42 can be deployed to any cloud provider, to your own servers, or to
embedded devices such as raspberry pi.

Proxy42 is at an early stage of development. Currently the API gateway supports

 - HTTP administrative control API
 - Mounting APIs on different paths under the same domain
 - Key based API authentication
 - Load balancing (Random pick strategy)
 - User defined custom authentication strategies
 - Basic header manipulation (Add or override headers in request or response)

Planned features on the roadmap include

 - Plugin based user defined strategies for rate limiting, load balancing,
   content transformations (Headers and body of requests and responses)
 - OAuth2 support, both as resource server and an authorization server
 - Analytics and logging
 - Additional predefined strategies for authentication, rate limiting and load balancing
 - HTTP/2 support on frontend and backend
