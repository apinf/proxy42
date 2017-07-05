-record(domain_group, {
          id,
          hostname,
          frontend_prefix,
          backend_prefix,
          servers,
          strategy,
          additional_headers,
          rate_limit,
          auth_config,
          developers}).
