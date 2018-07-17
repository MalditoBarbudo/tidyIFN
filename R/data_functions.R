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
    password = rstudioapi::askForPassword('Pasword for IFN db'),
    dbname = 'ifn',
    idleTimeout = 3600000
  )

  return(db_conn)
}

#' Scenario 1 data
#'
#' Load data for scenario 1, plots without breakdown
#'
#' This function generates the core data for scenario 1, plots without breakdown,
#' meaning the SQL table for plot results ("r_parcela_ifnX")
#'
#' @param data_sig tbl connected to the SQL db with the plot sig info (no collect)
#' @param ifn character indicating the IFN version to load.
#' @param db database connection
#'
#' @export
load_scenario1_data <- function(
  data_sig, ifn = 'ifn2', db = ifn_connect()
) {
  # table name
  core_name <- glue::glue('r_parcela_{ifn}')

  # res table, no collect
  res <- data_sig %>%
    dplyr::right_join(dplyr::tbl(db, core_name))

  return(res)
}

#' Scenario 2 data
#'
#' Load data for scenario 2, plots breakdown by functional group
#'
#' This function generates the core data for scenario 2, plots with breakdown by
#' functional group, meaning the SQL table for plot and group results
#' ("r_GROUP_ifnX")
#'
#' @param data_sig tbl connected to the SQL db with the plot sig info (no collect)
#' @param ifn character indicating the IFN version to load.
#' @param func_group character indicating the functional group to breakdown the
#'   plot data.
#' @param db database connection
#'
#' @export
load_scenario2_data <- function(
  data_sig, ifn = 'ifn2', func_group = 'especie', db = ifn_connect()
) {
  # table name
  core_name <- glue::glue('r_{func_group}_{ifn}')

  # res table, no collect
  res <- data_sig %>%
    dplyr::right_join(dplyr::tbl(db, core_name))

  return(res)
}

#' Scenario 3 data
#'
#' Load data for scenario 3, polygons without breakdown
#'
#' This function generates the core data for scenario 3,polygons without
#' breakdown, meaning the SQL table for plot results ("r_parcela_ifnX"), but
#' summarised by administrative level
#'
#' @param data_sig tbl connected to the SQL db with the plot sig info (no collect)
#' @param ifn character indicating the IFN version to load.
#' @param db database connection
#' @param ... filter expressions (as in dplyr::filter) to filter by dominant
#'   functional group, useful when using the data for the map visualization
#'
#' @export
load_scenario3_data <- function(
  data_sig, ifn = 'ifn2', db = ifn_connect(), ...
) {
  # enquos for the filters
  dots <- dplyr::enquo(...)

  # table name
  core_name <- glue::glue('r_parcela_{ifn}')

  # res table, no collect
  res <- data_sig %>%
    dplyr::right_join(dplyr::tbl(db, core_name))

  return(res)
}
