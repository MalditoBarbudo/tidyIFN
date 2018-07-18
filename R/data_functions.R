#' Creating a connection to the IFN SQL database
#'
#' Wrapper of \code{\link{dbPool}} to connect to the IFN database
#'
#' There is no arguments, this is used to access as a guest to the database
#'
#' @export
ifn_connect <- function() {
  db_conn <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    user = 'guest',
    password = 'guest',
    dbname = 'oracle_ifn',
    idleTimeout = 3600000
  )

  return(db_conn)
}

#' SIG data getter
#'
#' Function to load the SIG data
#'
#' This function does NOT collect the data from the db, only performs the steps
#' necessary to retrieve and filter the data if necessary from the db. The
#' collect step must be performed with the result of the function
#'
#' @param ifn character indicating the IFN version to load
#' @param db database connection
#' @param ... filter arguments to the dplyr::filter step
#'
#' @export
data_sig <- function(ifn, db = ifn_connect(), ...) {

  # enquo the filters
  dots <- dplyr::quos(...)

  # sig name
  name_sig <- glue::glue('parcela{ifn}_sig_etrs89')

  # sig data
  res <- dplyr::tbl(db, name_sig)

  # are there filters??
  if (!rlang::is_empty(dots)) {
    res <- res %>%
      dplyr::filter(!!! dots)
  }

  return(res)

}

#' Climatic data
#'
#' Function to load the climatic data, from the db tables
#'
#' This function loads the climatic data for those plots selected in data_sig
#'
#' @param data_sig tbl connection, generally the result of data_sig function
#' @param ifn character indicating the IFN version to load
#' @param db database connection
#' @param ... filter arguments to the dplyr::filter step
#'
#' @export
data_clima <- function(data_sig, ifn, db = ifn_connect(), ...) {

  # quos
  dots <- dplyr::quos(...)

  # name
  clima_name <- glue::glue('parcela{ifn}_clima')

  # res
  res <- data_sig %>%
    dplyr::select(idparcela) %>%
    dplyr::left_join(dplyr::tbl(db, clima_name), by = 'idparcela')

  # are there filters??
  if (!rlang::is_empty(dots)) {
    res <- res %>%
      dplyr::filter(!!! dots)
  }

  return(res)
}

#' Core data
#'
#' Function to load the "core" data, data from db tables
#'
#' This function takes the result of data_sig to retrieve the core data at the
#' breakdown level for the plots selected
#'
#' @param data_sig tbl connection, generally the result of data_sig function
#' @param ifn character indicating the IFN version to load
#' @param func_group character indicating the breakdown level (parcela, especie,
#'   especiesimpl, genere, planifconif, caducesclerconif)
#' @param db db connection
#' @param clima_plots plots resulting from the filtered clima data
#'
#' @export
data_core <- function(
  data_sig, ifn = 'ifn2', func_group = 'parcela', db = ifn_connect(), clima_plots
) {
  # table name
  core_name <- glue::glue('r_{func_group}_{ifn}')

  # res table, no collect
  res <- data_sig %>%
    dplyr::select(idparcela, provincia, vegueria, comarca, municipi) %>%
    dplyr::filter(idparcela %in% clima_plots) %>%
    dplyr::left_join(dplyr::tbl(db, core_name), by = 'idparcela')

  return(res)
}

#' Summarise the polygons
#'
#' Function to summarise variable values for each selected polygon
#'
#' This function takes the core data for the plots and calculate the summaries
#' based on the polygon kind selected. It also can be filtered before summarising
#' in the case of dominant functional groups
#'
#' @param data_core tbl con, usually obtained from data_core() function
#' @param polygon_group character indicating the polygon group to summarise
#' @param func_group character indicating the functional group to also summarise,
#'   if any.
#' @param .funs funs (dplyr) object with the functions to summarise
#' @param ... filter arguments
#'
#' @export
summarise_polygons <- function(
  data_core, polygon_group,
  func_group = '',
  .funs = dplyr::funs(
    mean(., na.rm = TRUE),
    stats::sd(., na.rm = TRUE),
    min(., na.rm = TRUE),
    max(., na.rm = TRUE),
    stats::median(., na.rm = TRUE),
    q95 = stats::quantile(., probs = 0.95, na.rm = TRUE),
    dplyr::n()
  ),
  ...
) {

  # quos
  dots <- dplyr::quos(...)
  if (rlang::is_empty(dots)) {
    dots <- dplyr::quo(TRUE)
  }

  grouping_vars <- dplyr::quos(
    !!dplyr::sym(polygon_group), !!dplyr::sym(func_group)
  )
  grouping_vars <- grouping_vars[!vapply(
    grouping_vars, rlang::quo_is_missing, logical(1)
  )]

  res <- data_core %>%
    dplyr::group_by(!!! grouping_vars) %>%
    dplyr::filter(!!! dots) %>%
    dplyr::collect() %>%
    dplyr::summarise_if(is.numeric, .funs = .funs)

  return(res)

}
