# download_onpe_ubigeo_v14.R
#
# Download ONPE "resumen-general" data for a single Peruvian UBIGEO using the
# URL pattern confirmed manually in the browser.
#
# v14 keeps the v10 NULL-to-NA fix and enriches `participantes` with:
# - first column: `ubigeo`
# - second column: `actasContabilizadas`, copied from `totales`
#
# Required packages:
# install.packages(c("httr2", "jsonlite"))
#
#
# Geographic scope:
# - UBIGEOs with first two digits >= 90 are treated as Extranjero and use
#   idAmbitoGeografico = 2, e.g. 910101 or 940302.
# - All other UBIGEOs use idAmbitoGeografico = 1.
# - You can override this with `id_ambito_geografico = 1` or `2`.
# Minimal use with copied cURL:
#
# source("download_onpe_ubigeo_v14.R")
#
# curl_txt = r"(PASTE THE COPIED CURL COMMAND HERE)"
# headers = headers_from_curl(curl_txt)
#
# x = download_onpe_ubigeo_v14(
#   ubigeo = "010205",
#   headers = headers,
#   header_mode = "curl_only",
#   save_csv = TRUE,
#   save_json = TRUE,
#   output_dir = "_scratch"
# )
#
# x$query_urls
# x$params
# x$data$totales
# x$data$participantes
#
# Bulk use:
#
# all_data = download_onpe_ubigeos_v14(
#   ubigeos = c("010205", "010206"),
#   headers = headers
# )
#
# This saves individual files in `_scratch/` by default, writes
# `onpe_all_totales.csv` and `onpe_all_participantes.csv` in the working
# directory, and returns the combined data invisibly.


build_onpe_ubigeo_params = function(ubigeo,
                                    id_eleccion = 10,
                                    id_ambito_geografico = NULL) {
  ubigeo = as.character(ubigeo)

  if (!grepl("^[0-9]{6}$", ubigeo)) {
    stop("`ubigeo` must be a 6-digit character string, for example '010205' or '910101'.",
         call. = FALSE)
  }

  # ONPE uses:
  #   idAmbitoGeografico = 1 for Peru
  #   idAmbitoGeografico = 2 for Extranjero
  #
  # Foreign UBIGEOs use high two-digit department-like blocks, e.g.:
  #   91xxxx = EXTRANJERO / AFRICA
  #   94xxxx = EXTRANJERO / EUROPA
  #
  # Peru uses ordinary department codes such as 01, 02, ..., 25.
  # The automatic rule therefore treats first-two-digit codes >= 90 as
  # Extranjero.
  if (is.null(id_ambito_geografico)) {
    id_ambito_geografico = if (as.integer(substr(ubigeo, 1, 2)) >= 90) 2 else 1
  }

  id_ubigeo_departamento = paste0(substr(ubigeo, 1, 2), "0000")
  id_ubigeo_provincia = paste0(substr(ubigeo, 1, 4), "00")

  if (substr(ubigeo, 3, 6) == "0000") {
    return(list(
      idEleccion = id_eleccion,
      tipoFiltro = "ubigeo_nivel_01",
      idAmbitoGeografico = id_ambito_geografico,
      idUbigeoDepartamento = id_ubigeo_departamento
    ))
  }

  if (substr(ubigeo, 5, 6) == "00") {
    return(list(
      idEleccion = id_eleccion,
      tipoFiltro = "ubigeo_nivel_02",
      idAmbitoGeografico = id_ambito_geografico,
      idUbigeoDepartamento = id_ubigeo_departamento,
      idUbigeoProvincia = id_ubigeo_provincia
    ))
  }

  list(
    idEleccion = id_eleccion,
    tipoFiltro = "ubigeo_nivel_03",
    idAmbitoGeografico = id_ambito_geografico,
    idUbigeoDepartamento = id_ubigeo_departamento,
    idUbigeoProvincia = id_ubigeo_provincia,
    idUbigeoDistrito = ubigeo
  )
}


url_query_string = function(params) {
  paste(
    paste0(
      utils::URLencode(names(params), reserved = TRUE),
      "=",
      vapply(
        params,
        function(x) utils::URLencode(as.character(x), reserved = TRUE),
        character(1)
      )
    ),
    collapse = "&"
  )
}


onpe_req_url = function(endpoint,
                        params,
                        base_url = "https://resultadosegundavuelta.onpe.gob.pe/presentacion-backend/resumen-general") {
  endpoint = match.arg(endpoint, c("totales", "participantes"))

  paste0(
    sub("/+$", "", base_url),
    "/",
    endpoint,
    "?",
    url_query_string(params)
  )
}


parse_onpe_response = function(body, requested_url = NULL) {
  body = trimws(body)

  if (grepl("^\\s*<", body)) {
    stop(
      paste0(
        "ONPE returned HTML, not JSON.",
        if (!is.null(requested_url)) paste0("\n\nRequested URL:\n", requested_url) else "",
        "\n\nFirst 500 characters returned:\n",
        substr(body, 1, 500)
      ),
      call. = FALSE
    )
  }

  if (!grepl("^\\s*[\\{\\[]", body)) {
    stop(
      paste0(
        "ONPE returned a non-JSON-looking response.",
        if (!is.null(requested_url)) paste0("\n\nRequested URL:\n", requested_url) else "",
        "\n\nFirst 500 characters returned:\n",
        substr(body, 1, 500)
      ),
      call. = FALSE
    )
  }

  parsed = tryCatch(
    jsonlite::fromJSON(body, simplifyVector = TRUE),
    error = function(e) {
      stop(
        paste0(
          "Could not parse ONPE response as JSON.",
          if (!is.null(requested_url)) paste0("\n\nRequested URL:\n", requested_url) else "",
          "\n\nFirst 500 characters returned:\n",
          substr(body, 1, 500)
        ),
        call. = FALSE
      )
    }
  )

  if (is.list(parsed) && "data" %in% names(parsed)) {
    return(parsed$data)
  }

  parsed
}


normalise_header_names = function(headers) {
  if (is.null(headers)) {
    return(list())
  }

  if (!is.list(headers)) {
    stop("`headers` must be a named list.", call. = FALSE)
  }

  if (is.null(names(headers)) || any(!nzchar(names(headers)))) {
    stop("`headers` must be a named list.", call. = FALSE)
  }

  out = list()

  for (i in seq_along(headers)) {
    nm = names(headers)[[i]]
    value = as.character(headers[[i]])
    lower_nm = tolower(nm)

    existing = which(tolower(names(out)) == lower_nm)

    if (length(existing) > 0) {
      out[[existing[1]]] = NULL
    }

    out[[nm]] = value
  }

  out
}


headers_from_curl = function(curl_text) {
  curl_text = paste(curl_text, collapse = "\n")

  pattern = "-H\\s+(['\"])(.*?)\\1"
  m = gregexpr(pattern, curl_text, perl = TRUE)
  hits = regmatches(curl_text, m)[[1]]

  headers = list()

  if (length(hits) > 0 && hits[1] != "") {
    for (hit in hits) {
      header = sub("^[-]H\\s+(['\"])(.*)\\1$", "\\2", hit, perl = TRUE)
      pos = regexpr(":", header, fixed = TRUE)[1]

      if (!is.na(pos) && pos > 1) {
        name = trimws(substr(header, 1, pos - 1))
        value = trimws(substr(header, pos + 1, nchar(header)))
        headers[[name]] = value
      }
    }
  }

  cookie_pattern = "(?:-b|--cookie)\\s+(['\"])(.*?)\\1"
  cm = gregexpr(cookie_pattern, curl_text, perl = TRUE)
  cookie_hits = regmatches(curl_text, cm)[[1]]

  if (length(cookie_hits) > 0 && cookie_hits[1] != "") {
    cookie = sub("^(?:-b|--cookie)\\s+(['\"])(.*)\\1$", "\\2", cookie_hits[[1]], perl = TRUE)
    headers[["Cookie"]] = cookie
  }

  normalise_header_names(headers)
}


default_onpe_headers = function(main_page = "https://resultadosegundavuelta.onpe.gob.pe/main/resumen") {
  chrome_ua = paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    "AppleWebKit/537.36 (KHTML, like Gecko)",
    "Chrome/149.0.0.0 Safari/537.36"
  )

  list(
    accept = "*/*",
    `accept-language` = "en-ZA,en;q=0.9,es-419;q=0.8,es;q=0.7,fr;q=0.6",
    `content-type` = "application/json",
    referer = main_page,
    `sec-ch-ua` = '"Google Chrome";v="149", "Chromium";v="149", "Not)A;Brand";v="24"',
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = '"Windows"',
    `sec-fetch-dest` = "empty",
    `sec-fetch-mode` = "cors",
    `sec-fetch-site` = "same-origin",
    `user-agent` = chrome_ua
  )
}


merge_headers = function(default_headers, user_headers = NULL) {
  default_headers = normalise_header_names(default_headers)

  if (is.null(user_headers)) {
    return(default_headers)
  }

  user_headers = normalise_header_names(user_headers)

  out = default_headers

  for (nm in names(user_headers)) {
    lower_nm = tolower(nm)
    existing = which(tolower(names(out)) == lower_nm)

    if (length(existing) > 0) {
      out[[existing[1]]] = NULL
    }

    out[[nm]] = user_headers[[nm]]
  }

  normalise_header_names(out)
}


perform_onpe_request = function(endpoint,
                                params,
                                headers = NULL,
                                header_mode = c("default", "curl_only", "merge"),
                                base_url = "https://resultadosegundavuelta.onpe.gob.pe/presentacion-backend/resumen-general",
                                timeout_seconds = 60) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package `httr2` is required. Install it with install.packages('httr2').",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required. Install it with install.packages('jsonlite').",
         call. = FALSE)
  }

  header_mode = match.arg(header_mode)
  endpoint = match.arg(endpoint, c("totales", "participantes"))

  req_headers_list = switch(
    header_mode,
    default = default_onpe_headers(),
    curl_only = normalise_header_names(headers),
    merge = merge_headers(default_onpe_headers(), headers)
  )

  requested_url = onpe_req_url(endpoint, params, base_url = base_url)

  req = httr2::request(paste0(sub("/+$", "", base_url), "/", endpoint))
  req = do.call(httr2::req_url_query, c(list(req), params))
  req = do.call(httr2::req_headers, c(list(req), req_headers_list))
  req = httr2::req_timeout(req, timeout_seconds)

  resp = httr2::req_perform(req)
  status = httr2::resp_status(resp)
  body = httr2::resp_body_string(resp)

  if (status < 200 || status >= 300) {
    stop(
      paste0(
        "ONPE request failed with HTTP status ", status, ".\n\n",
        "Requested URL:\n", requested_url, "\n\n",
        "First 500 characters returned:\n",
        substr(body, 1, 500)
      ),
      call. = FALSE
    )
  }

  parse_onpe_response(body, requested_url = requested_url)
}


replace_nulls = function(x) {
  if (is.null(x)) {
    return(NA)
  }

  if (is.list(x) && !is.data.frame(x)) {
    return(lapply(x, replace_nulls))
  }

  x
}


is_scalar_record = function(x) {
  is.list(x) &&
    !is.null(names(x)) &&
    all(nzchar(names(x))) &&
    all(vapply(
      x,
      function(z) {
        is.null(z) ||
          (is.atomic(z) && length(z) <= 1) ||
          (is.list(z) && length(z) == 0)
      },
      logical(1)
    ))
}


as_onpe_table = function(x) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required. Install it with install.packages('jsonlite').",
         call. = FALSE)
  }

  x = replace_nulls(x)

  if (is.data.frame(x)) {
    return(jsonlite::flatten(x))
  }

  if (is.atomic(x) && !is.null(names(x))) {
    return(as.data.frame(as.list(x), stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (is_scalar_record(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (is.list(x)) {
    # Prefer JSON normalisation for list-of-records, such as `participantes`.
    y = jsonlite::fromJSON(
      jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"),
      flatten = TRUE,
      simplifyVector = TRUE
    )

    y = replace_nulls(y)

    if (is.data.frame(y)) {
      return(jsonlite::flatten(y))
    }

    if (is_scalar_record(y)) {
      return(as.data.frame(y, stringsAsFactors = FALSE, check.names = FALSE))
    }

    if (is.list(y)) {
      return(as.data.frame(y, stringsAsFactors = FALSE, check.names = FALSE))
    }
  }

  as.data.frame(x, stringsAsFactors = FALSE)
}


augment_participantes_table = function(participantes,
                                       totales,
                                       ubigeo) {
  participantes_table = as_onpe_table(participantes)

  if (is.list(totales) && "actasContabilizadas" %in% names(totales)) {
    actas_contabilizadas = totales[["actasContabilizadas"]]
  } else if (is.data.frame(totales) && "actasContabilizadas" %in% names(totales)) {
    actas_contabilizadas = totales[["actasContabilizadas"]][1]
  } else {
    actas_contabilizadas = NA
  }

  prefix = data.frame(
    ubigeo = rep(as.character(ubigeo), nrow(participantes_table)),
    actasContabilizadas = rep(actas_contabilizadas, nrow(participantes_table)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  cbind(prefix, participantes_table)
}


download_onpe_ubigeo_v14 = function(ubigeo,
                                    id_eleccion = 10,
                                    id_ambito_geografico = NULL,
                                    endpoints = c("totales", "participantes"),
                                    headers = NULL,
                                    header_mode = c("default", "curl_only", "merge"),
                                    save_csv = FALSE,
                                    save_json = FALSE,
                                    output_dir = ".",
                                    file_prefix = NULL,
                                    sleep_seconds = 0.2) {
  header_mode = match.arg(header_mode)
  endpoints = match.arg(endpoints, c("totales", "participantes"), several.ok = TRUE)

  if (header_mode == "curl_only" && is.null(headers)) {
    stop("`headers` must be supplied when `header_mode = 'curl_only'`.",
         call. = FALSE)
  }

  params = build_onpe_ubigeo_params(
    ubigeo = ubigeo,
    id_eleccion = id_eleccion,
    id_ambito_geografico = id_ambito_geografico
  )

  data = stats::setNames(vector("list", length(endpoints)), endpoints)

  for (endpoint in endpoints) {
    data[[endpoint]] = perform_onpe_request(
      endpoint = endpoint,
      params = params,
      headers = headers,
      header_mode = header_mode
    )

    if (!is.null(sleep_seconds) && sleep_seconds > 0) {
      Sys.sleep(sleep_seconds)
    }
  }

  if (all(c("totales", "participantes") %in% names(data))) {
    data[["participantes"]] = augment_participantes_table(
      participantes = data[["participantes"]],
      totales = data[["totales"]],
      ubigeo = ubigeo
    )
  }

  result = list(
    ubigeo = as.character(ubigeo),
    params = params,
    query_urls = stats::setNames(
      vapply(endpoints, function(endpoint) onpe_req_url(endpoint, params), character(1)),
      endpoints
    ),
    header_mode = header_mode,
    data = data
  )

  if (save_csv || save_json) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    if (is.null(file_prefix)) {
      file_prefix = paste0("onpe_", as.character(ubigeo))
    }

    if (save_json) {
      jsonlite::write_json(
        result,
        path = file.path(output_dir, paste0(file_prefix, ".json")),
        pretty = TRUE,
        auto_unbox = TRUE,
        null = "null"
      )
    }

    if (save_csv) {
      for (endpoint in endpoints) {
        tab = as_onpe_table(data[[endpoint]])
        utils::write.csv(
          tab,
          file = file.path(output_dir, paste0(file_prefix, "_", endpoint, ".csv")),
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )
      }
    }
  }

  invisible(result)
}


bind_onpe_rows = function(x) {
  if (length(x) == 0) {
    return(data.frame())
  }

  x = lapply(x, function(z) {
    if (is.null(z)) {
      return(NULL)
    }

    z = as_onpe_table(z)
    z[] = lapply(z, function(col) {
      if (is.factor(col)) {
        as.character(col)
      } else {
        col
      }
    })

    z
  })

  x = Filter(Negate(is.null), x)

  if (length(x) == 0) {
    return(data.frame())
  }

  all_names = unique(unlist(lapply(x, names)))

  x = lapply(x, function(z) {
    missing_names = setdiff(all_names, names(z))

    for (nm in missing_names) {
      z[[nm]] = NA
    }

    z = z[, all_names, drop = FALSE]
    z
  })

  do.call(rbind, x)
}


download_onpe_ubigeos_v14 = function(ubigeos,
                                     headers,
                                     id_eleccion = 10,
                                     id_ambito_geografico = NULL,
                                     header_mode = c("curl_only", "default", "merge"),
                                     individual_output_dir = "_scratch",
                                     save_individual_csv = TRUE,
                                     save_individual_json = TRUE,
                                     save_combined_csv = TRUE,
                                     combined_output_dir = ".",
                                     combined_file_prefix = "onpe_all",
                                     sleep_seconds = 0.2,
                                     continue_on_error = TRUE,
                                     verbose = TRUE) {
  header_mode = match.arg(header_mode)

  if (missing(ubigeos) || is.null(ubigeos)) {
    stop("`ubigeos` must be supplied as a character vector, for example c('010205', '010206').",
         call. = FALSE)
  }

  ubigeos = unique(as.character(ubigeos))
  ubigeos = ubigeos[nzchar(ubigeos)]

  if (length(ubigeos) == 0) {
    stop("`ubigeos` is empty after removing blank values.", call. = FALSE)
  }

  if (header_mode == "curl_only" && missing(headers)) {
    stop("`headers` must be supplied when `header_mode = 'curl_only'`.",
         call. = FALSE)
  }

  dir.create(individual_output_dir, showWarnings = FALSE, recursive = TRUE)

  results = vector("list", length(ubigeos))
  names(results) = ubigeos

  failures = data.frame(
    ubigeo = character(),
    error = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(ubigeos)) {
    ubigeo = ubigeos[[i]]

    if (isTRUE(verbose)) {
      message(sprintf("[%d/%d] Downloading UBIGEO %s", i, length(ubigeos), ubigeo))
    }

    result = tryCatch(
      download_onpe_ubigeo(
        ubigeo = ubigeo,
        id_eleccion = id_eleccion,
        id_ambito_geografico = id_ambito_geografico,
        endpoints = c("totales", "participantes"),
        headers = headers,
        header_mode = header_mode,
        save_csv = save_individual_csv,
        save_json = save_individual_json,
        output_dir = individual_output_dir,
        sleep_seconds = sleep_seconds
      ),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      Sys.sleep(0.05)
      failures = rbind(
        failures,
        data.frame(
          ubigeo = ubigeo,
          error = conditionMessage(result),
          stringsAsFactors = FALSE
        )
      )

      if (!isTRUE(continue_on_error)) {
        stop(result)
      }

      if (isTRUE(verbose)) {
        message("  failed: ", conditionMessage(result))
      }

      results[[ubigeo]] = NULL
      next
    }

    results[[ubigeo]] = result
  }

  ok_results = Filter(Negate(is.null), results)

  totales_list = lapply(ok_results, function(x) {
    tab = as_onpe_table(x$data$totales)

    if (!"ubigeo" %in% names(tab)) {
      tab = cbind(
        data.frame(ubigeo = x$ubigeo, stringsAsFactors = FALSE),
        tab
      )
    }

    tab
  })

  participantes_list = lapply(ok_results, function(x) {
    as_onpe_table(x$data$participantes)
  })

  totales = bind_onpe_rows(totales_list)
  participantes = bind_onpe_rows(participantes_list)

  out = list(
    totales = totales,
    participantes = participantes,
    individual_results = ok_results,
    failures = failures,
    ubigeos_requested = ubigeos,
    ubigeos_downloaded = names(ok_results)
  )

  if (isTRUE(save_combined_csv)) {
    dir.create(combined_output_dir, showWarnings = FALSE, recursive = TRUE)

    utils::write.csv(
      totales,
      file = file.path(combined_output_dir, paste0(combined_file_prefix, "_totales.csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )

    utils::write.csv(
      participantes,
      file = file.path(combined_output_dir, paste0(combined_file_prefix, "_participantes.csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )

    if (nrow(failures) > 0) {
      utils::write.csv(
        failures,
        file = file.path(combined_output_dir, paste0(combined_file_prefix, "_failures.csv")),
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  }

  invisible(out)
}


# Convenience alias. Use the v14 name if you want to avoid conflicts with older
# functions already loaded in the R session.
download_onpe_ubigeo = download_onpe_ubigeo_v14
download_onpe_ubigeos = download_onpe_ubigeos_v14
