otel_tracer_name <- "r.package.reqres"

get_tracer <- local({
  tracer <- NULL
  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }
    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_tracer())
    }
    tracer <<- otel::get_tracer()
    tracer
  }
})

testthat__is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

get_meter <- local({
  meter <- NULL
  function() {
    if (!is.null(meter)) {
      return(meter)
    }
    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_meter())
    }
    meter <<- otel::get_meter()
    meter
  }
})
metric_attributes <- function(request, response) {
  http_server_target <- derive_http_server_target(request)
  list2(
    http.request.method = toupper(request$method),
    url.scheme = http_server_target$scheme,
    !!!if (response$status >= 500) {
      list(error.type = as.character(response$status))
    },
    http.response.status_code = response$status,
    network.protocol.name = "http",
    network.protocol.version = "1.1",
    server.address = http_server_target$address,
    server.port = http_server_target$port
  )
}
record_duration <- function(request, attributes) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::histogram_record(
      "http.server.request.duration",
      request$duration,
      attributes = attributes,
      context = request$otel,
      meter = meter
    )
  }
}
push_active_request <- function(request) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    http_server_target <- derive_http_server_target(request)
    otel::up_down_counter_add(
      "http.server.active_requests",
      value = 1L,
      attributes = list(
        http.request.method = toupper(request$method),
        url.scheme = http_server_target$scheme,
        server.address = http_server_target$address,
        server.port = http_server_target$port
      ),
      context = request$otel,
      meter = meter
    )
  }
}
pop_active_request <- function(request, attributes) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::up_down_counter_add(
      "http.server.active_requests",
      value = -1L,
      attributes = attributes[
        c("http.request.method", "url.scheme", "server.address", "server.port")
      ],
      context = request$otel,
      meter = meter
    )
  }
}
record_request_body <- function(request, attributes) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::histogram_record(
      "http.server.request.body.size",
      value = as.numeric(request$headers$content_length %||% 0),
      attributes = attributes,
      context = request$otel,
      meter = meter
    )
  }
}
record_response_body <- function(request, response, attributes) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::histogram_record(
      "http.server.response.body.size",
      value = if (is.raw(response$body)) {
        length(response$body)
      } else {
        nchar(response$body, "bytes")
      },
      attributes = attributes,
      context = request$otel,
      meter = meter
    )
  }
}


request_ospan <- function(request, start_time, tracer) {
  # OpenTelemetry
  # network.local.address and network.local.port must be set by the framework
  # http.response.status_code and http.response.header.<key> can only be set later
  http_server_target <- derive_http_server_target(request)
  span <- otel::start_span(
    tolower(paste0(request$method, "_", request$path)),
    options = list(
      start_system_time = start_time,
      kind = "server",
      parent = otel::extract_http_context(request$headers)
    ),
    attributes = list2(
      http.request.method = toupper(request$method),
      url.path = request$path,
      url.scheme = http_server_target$scheme,
      network.protocol.name = "http",
      server.port = http_server_target$port,
      url.query = request$querystring,
      client.address = http_server_target$address,
      network.protocol.version = "1.1",
      server.address = sub("^(.*):.*$", "\\1", request$host),
      user_agent.original = request$headers[["user_agent"]],
      !!!set_names(
        request$headers,
        paste0(
          "http.request.header.",
          gsub("_", "-", names(request$headers))
        )
      )
    ),
    tracer = tracer
  )
}

derive_http_server_target <- function(request) {
  headers <- request$headers
  forwarded <- headers[["forwarded"]]
  # 1. If Forwarded
  if (!is.null(forwarded) && nzchar(forwarded)) {
    first <- strsplit(forwarded, ",", fixed = TRUE)[[1]][1]
    key_values <- regmatches(first, gregexpr("([A-Za-z-]+)=(\"[^\"]*\"|[^;]+)", first))[[1]]
    pairs <- strsplit(key_values, "=", fixed = TRUE)
    params <- setNames(
      vapply(pairs, function(pair) gsub('"', '', pair[[2]], fixed = TRUE), character(1)),
      tolower(vapply(pairs, `[[`, character(1), 1L))
    )
    if (!is.null(params[["host"]])) {
      return(list(
        address = sub(":[0-9]+$", "", params[["host"]]),
        port = port_or_default(params[["host"]], params[["proto"]] %||% request$protocol),
        scheme = params[["proto"]] %||% request$protocol
      ))
    }
  }
  # 2. X-Forwarded-*
  x_forwarded_host <- headers[["x_forwarded_host"]]
  x_forwarded_proto <- headers[["x_forwarded_proto"]]
  x_forwarded_port <- headers[["x_forwarded_port"]]
  if (!is.null(x_forwarded_host)) {
    scheme <- x_forwarded_proto %||% request$protocol
    host   <- strsplit(x_forwarded_host, ",", fixed = TRUE)[[1]][1]
    port <- if (!is.null(x_forwarded_port)) {
      as.integer(strsplit(x_forwarded_port, ",", fixed = TRUE)[[1]][1])
    } else {
      port_or_default(host, scheme)
    }
    return(list(address = sub(":[0-9]+$", "", host), port = port, scheme = scheme))
  }
  # 3. Host header
  list(
    address = sub(":[0-9]+$", "", request$host),
    port = port_or_default(request$host, request$protocol),
    scheme = request$protocol
  )
}

port_or_default <- function(host, scheme) {
  m <- regmatches(host, regexec(":([0-9]+)$", host))[[1]]
  if (length(m) == 2L) {
    as.integer(m[[2]])
  } else if (identical(scheme, "https")) {
    443L
  } else {
    80L
  }
}
