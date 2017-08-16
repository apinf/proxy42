Prerequisites
=============

Installing elixir
-----------------

Ensure that elixir is installed. The instructions for installing elixir
can be found at https://elixir-lang.org/install.html ``iex`` and ``mix``
should be available before proceeding further.

Getting the source
------------------

The source can be obtained by cloning the git repository available at
https://github.com/apinf/proxy42

.. code:: shell

       git clone https://github.com/apinf/proxy42

Alternatively, a zip or a tarball of the source can be obtained from the
same url.

Building the project
====================

The following command will fetch dependencies, build the project and run
proxy42 in a development

.. code:: shell

    iex -S mix do deps.get, compile, run

TODO Building for production
----------------------------

Awaiting deadlines or similar auspicious occasions.

TODO Building for embedded devices
----------------------------------

https://github.com/fhunleth/fwup#installing

Install ssh-askpass squashfs-tools
*Hand wave* Build firmware, burn, and boot.

Trying it out
=============

This section is expected to change relatively frequently as the admin
API is iterated on. This document will be updated as and when
appropriate.

The current version of the "api" is extremely clunky, and is expected to
be overhauled soon.

Registering a developer
-----------------------

A developer can be registered by sending a POST request with an empty
body to localhost:4001/developers

.. code:: shell

       curl localhost:4001/developers

The response will provide a developer id and an API key. e.g

.. code:: json

       {
       "id": "d8d63fd0-3987-48dc-9ac2-2f2f5fe49e92", 
       "key": "311691e7-8f47-45eb-b606-9bc5c23ba7a9"
       }

Intended API:
~~~~~~~~~~~~~

The POST body would accept additional metadata related to the developer.
The authentication strategy should be decoupled from developer account.
When key based authentication is an option, it should be possible to
associate multiple keys with a developer account.

Developer registration *may* require additional steps, such as email
verification, or may require authentication and be limited to proxy
administrators.

Registering an API
------------------

An API is registered with the proxy as follows:

.. code:: shell

        curl -vvv -XPOST -H "Content-Type: application/json" -d
        '{"hostname": "example.org", "servers" : ["http://whatever.domain",
        "http://1.2.3.4:5678"],
        "frontend_prefix": "/awesome-api/",
        "backend_prefix": "/does-this-work/",
        "strategy": "random", "rate_limit": 43,
        "additional_headers" : "", "developers": ["d8d63fd0-3987-48dc-9ac2-2f2f5fe49e92"]
        }' http://localhost:4001/apis

The above registers an api where:

-  all requests to ``$PROXY_BASE/awesome-api/`` will be sent to
-  a random server from the servers listed for ``whatever.domain`` in DNS
   and ``1.2.3.4:5678``
-  after replacing ``/awesome-api/`` with ``/does-this-work`` in the url
-  and setting the Host header to example.org
-  and the developer with id "d8d6..." is the only one allowed to make
   such requests.

``rate_limit`` is required but not validated and enforced right now.

Intended API:
~~~~~~~~~~~~~

Associating developers with apis should be decoupled from registering
apis itself. A convenience may be allowed to preassign allowed
developers. Registering an api SHOULD require authentication and
appropriate permissions.

Testing the API
---------------

.. code:: shell

        curl -vvv -H "Authorization: Bearer 311691e7-8f47-45eb-b606-9bc5c23ba7a9" $PROXY_BASE/1500/

``$PROXY_BASE`` would be ``localhost:8080`` when testing locally, or
``<domain-name-or-ip>:8080`` if the proxy is running on a different machine.

This request will be allowed, and the response would be forwarded back
to the client. Requests without the correct API key would receive a 401
Unauthorized response.