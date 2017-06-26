- [API](#sec-1)
- [API Owner](#sec-2)
- [API Developer](#sec-3)
- [API End Users](#sec-4)
- [API Administrator](#sec-5)
- [API Proxy](#sec-6)
  - [Routing Rules](#sec-6-1)
    - [Domain based routing](#sec-6-1-1)
    - [Segment based routing](#sec-6-1-2)
    - [Path based routing](#sec-6-1-3)
  - [API Gateway Policies](#sec-6-2)
    - [Rate Limiting](#sec-6-2-1)
    - [Authentication](#sec-6-2-2)
    - [Authorization and Security Policies](#sec-6-2-3)
    - [Analytics](#sec-6-2-4)
    - [Request or Response Manipulation](#sec-6-2-5)
    - [Caching Policies](#sec-6-2-6)

These are preliminary design thoughts around the design of an API gateway. The following entities are involved. The terminology is in flux.

# API<a id="sec-1"></a>

These are the backend services that are behind the API gateway. Each API, for us, is essentially a set of backend servers that we route requests to. APIs are created by an API Owner.

# API Owner<a id="sec-2"></a>

API Owners create an API. They also monitor access to this API by API developers, and may want to limit, control or monetize this access.

# API Developer<a id="sec-3"></a>

API Developers develop against the API provided by the API Owner. They are represented by an API key.

# API End Users<a id="sec-4"></a>

API end users use the application developed by the API developer. An API developer might want to control usage by end-users, in order to meet their own requirements. They may be represented by a bearer token.

# API Administrator<a id="sec-5"></a>

For large enterprises, administrators might manage a number of API proxies on behalf of a set of API Owners. This distinction between administrator/owner might not be needed for smaller organizations and might not be made for the sake of simplicity.

# API Proxy<a id="sec-6"></a>

This is currently called a domain group in our source, that terminology being adapted from vegur, a reverse proxy that we build upon. An API Proxy connects one(or more?) API to a set of routing rules, and a set of policies.

## Routing Rules<a id="sec-6-1"></a>

### Domain based routing<a id="sec-6-1-1"></a>

We redirect custom domains to their own API. This is the simplest to do.

### Segment based routing<a id="sec-6-1-2"></a>

We redirect a single prefix segment of a path to their own API. This is also relatively simple to implement. This is currently implemented.

### Path based routing<a id="sec-6-1-3"></a>

We redirect an arbitrary prefix of a path to their own API. This requires us to maintain a data structure such as a trie. Since lookup will take up some time, its preferable that owners choose to use the other two methods of routing.

The main job of an API gateway is to enforce policies and provide statistics at the level of an API proxy.

## API Gateway Policies<a id="sec-6-2"></a>

### Rate Limiting<a id="sec-6-2-1"></a>

Rate Limits are to be enforced in two phases. API Owners might want to throttle API developers, and developers might want to throttle their own end users. This can be represented in two tables with composite keys of api proxy, developer and developer, users.

### Authentication<a id="sec-6-2-2"></a>

Authentication is currently implemented as an extra middleware layer in proxy42, before routing happens. The simplest form of authentication is an API key. This would be the first authentication strategy to be implemented. However API keys are quite weak, and username/password and OAuth2 can be supported. We also need to provide the flexibility for users to provide their own authentication strategy.

For security, sensitive data such as keys should be separated from other data in our database, and have separate security policies.

### Authorization and Security Policies<a id="sec-6-2-3"></a>

Once a developer/owner has authenticated himself, we need to authorize them for various roles. At the moment, this should be kept simple and we do not need to support fine-grained authorization. However we can have some policies based on IP addresses and based on domains/path.

Authentication and Authorization will be elaborated in further design documents.

### Analytics<a id="sec-6-2-4"></a>

TODO

Low Priority:

### Request or Response Manipulation<a id="sec-6-2-5"></a>

e.g XML to JSON, Template based Rewriting, Manipulation of headers and query strings.

### Caching Policies<a id="sec-6-2-6"></a>

This is likely to be a "can of worms".
