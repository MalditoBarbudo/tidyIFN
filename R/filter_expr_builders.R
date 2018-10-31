## Filter expresion functions

#' Custom polygon filter expresion builder
#'
#' @param gis_data
#' @param custom_polygon

custom_poly_filter_expr_builder <- function(
  gis_data, custom_polygon
) {
  tmp <- custom_polygon[['geometry']][['coordinates']] %>%
    purrr::flatten() %>%
    purrr::set_names(nm = 1:length(.)) %>%
    purrr::modify_depth(1, purrr::set_names, nm = c('long', 'lat')) %>%
    dplyr::bind_rows() %>%
    sp::Polygon()

  tmp <- sp::SpatialPolygons(list(sp::Polygons(list(tmp), 'custom_polygon')))

  is_in_poly_col <- gis_data %>%
    dplyr::select(longitude, latitude) %>%
    sp::SpatialPoints() %>%
    sp::over(tmp) %>%
    as.vector()
  is_in_poly_col <- dplyr::case_when(
    is_in_poly_col == 1 ~ TRUE,
    TRUE ~ FALSE
  )

  plots_codes <- gis_data[['idparcela']][is_in_poly_col]

  filter_quo <- rlang::quo(
    idparcela %in% plots_codes
  )

  return(filter_quo)

}

#' Administrative divisions filter expressions builder
#'
#' @param admin_div
#' @param admin_div_fil
admin_div_filter_expr_builder <- function(
  admin_div, admin_div_fil
) {
  if (is.null(admin_div_fil) ||
      any(admin_div_fil == '')) {
    filter_expr_admin <- rlang::quos(.ignore_empty = 'all')
  } else {
    filter_expr_admin <- rlang::quo(
      !!rlang::sym(admin_div) %in% !!admin_div_fil
    )
  }

  return(filter_expr_admin)
}

#' Protected spaces filter expressions builder
#'
#' @param espai_tipus
#' @param espai_tipus_fil
protected_spaces_filter_expr_builder <- function(
  espai_tipus, espai_tipus_fil
) {
  if (is.null(espai_tipus_fil) ||
      any(espai_tipus_fil == '')) {
    filter_expr_espai <- rlang::quos(.ignore_empty = 'all')
  } else {
    # here we need also to check for nomes protegits and sense proteccio
    # to be able to filter these cases
    if (any(espai_tipus_fil == "only_protected")) {
      filter_expr_espai <- rlang::quo(
        !(!!rlang::sym(espai_tipus) %in% c(
          "Sense Pein", "Sense protecció", "SenseXarxa"
        ))
      )
    } else {
      filter_expr_espai <- rlang::quo(
        !!rlang::sym(espai_tipus) %in% !!espai_tipus_fil
      )
    }
  }

  return(filter_expr_espai)
}

#' extra filters expressions builder
#'
#' tururu
#'
#' This function builds the filter expression for extra gis filters and
#' extra climatic filters.
#'
#' @param numeric_vars
#' @param categorical_vars
extra_filter_expr_builder <- function(
  numeric_vars, categorical_vars
) {

  # numeric_vars
  if (is.null(numeric_vars) || numeric_vars == '') {
    numeric_exprs <- rlang::quos(.ignore_empty = 'all')
  } else {
    numeric_exprs <- lapply(
      numeric_vars,
      function(var) {
        rlang::quo(
          dplyr::between(!!rlang::sym(var), input[[!!var]][1], input[[!!var]][2])
        )
      }
    )
  }

  # categorical vars
  if (is.null(categorical_vars) || categorical_vars == '') {
    categorical_exprs <- rlang::quos(.ignore_empty = 'all')
  } else {
    categorical_exprs <- lapply(
      categorical_vars,
      function(var) {
        rlang::quo(
          !!rlang::sym(var) %in% c(input[[!!var]])
        )
      }
    )
  }

  filter_quos <- c(numeric_exprs, categorical_exprs)
  return(filter_quos)
}
