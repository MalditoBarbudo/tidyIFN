## Helper functions

#' Get the current data scenario
#'
#' Return the current scenario as character
#'
#' There are four main scenarios to work with in the NFI data:
#' \itemize{
#'   \item{Scenario 1. Total by plot. In this scenario we can have the raw total
#'   and the dominant functional group (in two flavours, basal area or density)}
#'   \item{Scenario 2. Plot broken down by functional group, regardless the
#'   dominance}
#'   \item{Scenarios 3 and 4. The same as 1 and 2 but summarised by polygon,
#'   being the polygon the administrative divisions or custom polygons}
#' }
#'
#' @param viz_shape The shape to visualize, plots or polygons
#' @param agg_level The breakdown level, plot, species...
#'
#' @export
get_scenario <- function(viz_shape, agg_level) {
  if (viz_shape == 'parcela') {
    if (agg_level == 'parcela') {
      # parcelas y agregado por parcelas
      return('scenario1')
    } else {
      # parcelas y desglosado por tipo funcional
      return('scenario2')
    }
  } else {
    if (agg_level == 'parcela') {
      # poligonos agregados por parcelas
      return('scenario3')
    } else {
      # poligonos desglosados por tipo funcional
      return('scenario4')
    }
  }
}

#' Filter builders
#'
#' @param functional_group
#' @param dominance_type
#' @param vals
#'
#' @export
ifn_tables_filter_builder <- function(functional_group, dominance_type, vals) {
  filter_lht <- glue::glue(
    "{functional_group}_dom_perc{dominance_type}"
  )
  filter_quo <- rlang::quo(!! rlang::sym(filter_lht) %in% vals)
  return(filter_quo)
}

#' GIS, climatic and core data in a list
#'
#' gis, clima and core data in a list for the corresponding agg level
#'
#' @param conn pool object to access the tables
#' @param ifn character indicating the ifn level
#' @param agg_level breakdown level, plot, species...
#' @param diameter_classes logical indicating if diameter classes are required (breaking down
#'   the table by diameter classes). Default to FALSE.
#' @param functional_group Functional group to retrieve table for, if any.
#'   Default to NULL (no functional group). See details for more information
#'
#'
get_scenario_data <- function(
  conn,
  ifn,
  agg_level,
  diameter_classes = FALSE,
  functional_group = NULL,
  # gis filters
  administrative_division_filter = rlang::quo(.ignore_empty = 'all'),
  protected_spaces_filter = rlang::quo(.ignore_empty = 'all'),
  gis_extra_filter = rlang::quo(.ignore_empty = 'all'),
  custom_polygon_filter = rlang::quo(.ignore_empty = 'all'),
  # climatic filters
  clim_extra_filter = rlang::quo(.ignore_empty = 'all'),
  # core filters
  core_filter = rlang::quo(.ignore_empty = 'all')
) {

  # SIG data
  gis_data <- get_ifn_gis(
    conn, ifn,
    !!! administrative_division_filter,
    !!! protected_spaces_filter,
    !!! gis_extra_filter,
    !!! custom_polygon_filter
  )

  # Climatic data
  clima_data <- get_ifn_clim(
    conn, ifn,
    !!! clim_extra_filter
  )

  # core data
  core_data <- get_ifn_tables(
    conn, ifn, diameter_classes,
    functional_group,
    !!! core_filter
  )
  # TODO
  # 1. function for core filters

  res <- list(
    gis = gis_data, clim = clima_data, core = core_data
  ) %>%
    purrr::reduce(
      dplyr::left_join
    )
  return(res)
}
